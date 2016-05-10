{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Combinators ( module Combinators
                   , module Combinators.Class
                   ) where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Function
import Data.Maybe
import Data.Monoid

import Apply
import Combinators.Class

------------------------------------------------------------------------------------------
-- extracting an eventing widget
------------------------------------------------------------------------------------------

ecDyn :: (EventContainer t m a) => Dynamic t (m a) -> m a
ecDyn = ecJoin <=< dyn

ecDyn' :: (EventContainer t m a) => (x -> m a) -> Dynamic t x -> m a
ecDyn' f state = ecDyn =<< mapDyn f state

ec :: (EventContainer t m r) => m (Dynamic t (m r)) -> m r
ec = join . fmap ecDyn

------------------------------------------------------------------------------------------

-- use an eventing widget whenever the Dynamic is True
dWhen :: (EventContainer t m a) => Dynamic t Bool -> m a -> m a
dWhen test widget =
  ecDyn' (\t -> if t then widget else return ecNever) test

-- use an eventing widget whenever the Dynamic contains a value
dWhenJust :: (EventContainer t m a)
          => Dynamic t (Maybe x)
          -> (Dynamic t x -> m a)
          -> m a
dWhenJust maybeVal makeWidget = do
  val <- mapDyn maybeToList maybeVal
  -- NOTE: using Leftmostable instead of Catenable since there's
  --       at most one Event stream source at any time
  evtsDyn <- mapDyn ecLeftmost =<< simpleList val makeWidget
  return . ecSwitch . current $ evtsDyn

-- use an eventing widget whenever the Dynamic contains a value
dWhenJust' :: (EventContainer t m a) => Dynamic t (Maybe x) -> (x -> m a) -> m a
dWhenJust' maybeVal makeWidget =
  dWhenJust maybeVal f
  where f = ec . mapDyn makeWidget

-- use an eventing widget whenever the Prism/Traversal yields a value
withPreview :: forall t m e s a. (EventContainer t m e)
            => (Dynamic t a -> m e)
            -> Getting (First a) s a
            -> Dynamic t s
            -> m e
withPreview f prism superD =
  join $ dWhenJust <$> (preview prism @/ superD) <#> f

-- freeze the Dynamic at its first value
dFirst :: (MonadWidget t m)
       => Dynamic t (Maybe x) -- some stream of Maybes
       -> m (Dynamic t (Maybe x)) -- holds constant after the first Just
dFirst allD = do
  mFirst <- sample $ current allD
  eFirst <- headE . ffilter isJust $ updated allD
  case mFirst of Just x -> return $ constDyn mFirst
                 Nothing -> holdDyn Nothing eFirst

-- make an eventing widget with the first value of a Dynamic
dWithFirst :: (EventContainer t m a)
           => Dynamic t (Maybe x)
           -> (x -> m a)
           -> m a
dWithFirst allD makeWidget = flip dWhenJust' makeWidget =<< dFirst allD

-- create an eventing widget when the first event occurs
eWhenFirst :: (EventContainer t m a) => Event t b -> m a -> m a
eWhenFirst test widget = do
  test' <- holdDyn False (fmap (const True) test)
  dWhen test' widget

-- equivalent of Control.Monad's `when` for EventContainers
whenE :: (MonadWidget t m, Neverable a) => Bool -> m a -> m a
whenE test x = if test then x else return ecNever

-- `if` statement for combining eventing widgets
dIf :: (EventContainer t m a, EventContainer t m b) => Dynamic t Bool -> m a -> m b -> m (a, b)
dIf test true false = do
  trues <- dWhen test true
  notTest <- mapDyn not test
  falses <- dWhen notTest false
  return (trues, falses)

-- simpler `if` statement for combining eventing widgets
dIf' :: (EventContainer t m a) => Dynamic t Bool -> m a -> m a -> m a
dIf' test true false = do
  (trues, falses) <- dIf test true false
  return $ ecEither trues falses

data DynCase t s a where
  DCase :: Getting (First x) s x -> (Dynamic t x -> a) -> DynCase t s a

-- TODO: Multiple cases can be active at the same time.
--       If the resultant Event streams are coincident,
--       then ecLeftmost only keeps 1 of the simultaneous occurrences.
dCase :: (EventContainer t m a)
      => Dynamic t s
      -> [DynCase t s (m a)]
      -> m a
dCase dVal = fmap ecLeftmost . mapM (dHandleCase dVal)

dHandleCase :: (EventContainer t m a)
            => Dynamic t s
            -> DynCase t s (m a)
            -> m a
dHandleCase dVal (DCase l makeWidget) = do
  dSubVal <- mapDyn (preview l) dVal
  dWhenJust dSubVal makeWidget
  --join $ dWhenJust <$> (preview l @/ dVal) <#> makeWidget

-- NOTE: if you accidentally fmap(?) into a `Bubbling t r a`, the type error may
--         indicate that `Bubbling t r a` doesn't match `a`
-- TODO: figure out why the above happens even though `Bubbling t r`
--         has no explicit Functor/Applicative/Monad instance
data Bubbling t r a =
  Bubbling { _bBubble :: r
           , _bContents :: a
           } deriving (Show, Eq)
makeLenses ''Bubbling

instance (Neverable a, Neverable r) => Neverable (Bubbling t r a) where
  ecNever = Bubbling ecNever ecNever

instance (Catenable a, Catenable r) => Catenable (Bubbling t r a) where
  ecCat x y = Bubbling ((ecCat `on` _bBubble) x y) ((ecCat `on` _bContents) x y)

instance (Leftmostable a, Leftmostable r) => Leftmostable (Bubbling t r a) where
  ecLeftmost evts = Bubbling (ecLeftmost $ _bBubble <$> evts) (ecLeftmost $ _bContents <$> evts)

instance (Switchable t a, Switchable t r) => Switchable t (Bubbling t r a) where
  ecSwitch evts =
    Bubbling (ecSwitch $ _bBubble <$> evts) (ecSwitch $ _bContents <$> evts)
  ecSwitchPromptly e0 eUpdates =
    Bubbling <$> ecExtractWith' ecSwitchPromptly _bBubble e0 eUpdates
    <*> ecExtractWith' ecSwitchPromptly _bContents e0 eUpdates
  ecSwitchPromptlyDyn dVal =
    Bubbling <$> ecExtractWith'' ecSwitchPromptlyDyn _bBubble dVal
    <*> ecExtractWith'' ecSwitchPromptlyDyn _bContents dVal

instance (EventContainer t m a, EventContainer t m r) => EventContainer t m (Bubbling t r a) where
  ecJoin ecEvt =
    Bubbling <$> ecExtractWith ecJoin _bBubble ecEvt
    <*> ecExtractWith ecJoin _bContents ecEvt

rJoin :: (Reflex t, Catenable r) => Bubbling t r (Bubbling t r a) -> Bubbling t r a
rJoin (Bubbling r1 (Bubbling r2 a)) = Bubbling (r1 `ecCat` r2) a

-- TODO: find out performance/optimization implications of all this never+mappending
class (Reflex t) => BubblingContainer t r c where
  type RContents c :: *
  rcFactor :: c -> Bubbling t r (RContents c)

instance (Reflex t, Catenable r) => BubblingContainer t r (Bubbling t r a, Bubbling t r b) where
  type RContents (Bubbling t r a, Bubbling t r b) = (a, b)
  rcFactor (ra, rb) =
    Bubbling (view bBubble ra `ecCat` view bBubble rb) (ra ^. bContents, rb ^. bContents)

instance (Reflex t, Catenable r) => BubblingContainer t r [Bubbling t r a] where
  type RContents [Bubbling t r a] = [a]
  rcFactor ras = Bubbling (ecConcat $ fmap _bBubble ras) (fmap _bContents ras)

-- TODO: typeclass for automatically lifting a sub-r into r
--       e.g. `r` is giant sum type of various global actions (like flux)
rdIf :: (EventContainer' t m r, EventContainer' t m a, EventContainer' t m b)
     => Dynamic t Bool
     -> m (Bubbling t r a)
     -> m (Bubbling t r b)
     -> m (Bubbling t r (a, b))
rdIf test true false = fmap rcFactor $ dIf test true false

neverBubbling :: (Neverable r) => a -> Bubbling t r a
neverBubbling = Bubbling ecNever

onlyBubbling :: Bubbling t r a -> Bubbling t r ()
onlyBubbling = set bContents ()

bubbling :: (Reflex t) => r -> Bubbling t r ()
bubbling = flip Bubbling ()

bLiftWith :: (Neverable s)
          => ASetter' s a
          -> Bubbling t a x
          -> Bubbling t s x
bLiftWith l bub =
  bub & bBubble .~ ecNever & bBubble . l .~ (bub ^. bBubble)

bMergeWith :: (Catenable a)
           => ASetter' s a
           -> (x -> y -> z)
           -> Bubbling t a x
           -> Bubbling t s y
           -> Bubbling t s z
bMergeWith l combine bub superBub =
  superBub & bBubble . l %~ ecCat (bub ^. bBubble)
  & bContents %~ combine (bub ^. bContents)

bMerge :: (Catenable a)
       => (x -> y -> z)
       -> Bubbling t a x
       -> Bubbling t a y
       -> Bubbling t a z
bMerge combine bub1 bub2 =
  bub2 & bBubble %~ ecCat (bub1 ^. bBubble)
  & bContents %~ combine (bub1 ^. bContents)

bubble :: r -> Bubbling t r ()
bubble = flip Bubbling ()

bubbleWith :: (Neverable s)
           => ASetter' s a
           -> a
           -> Bubbling t s ()
bubbleWith l = bLiftWith l . bubble

------------------------------------------------------------------------------------------
-- working with Catenable
------------------------------------------------------------------------------------------

-- TODO: typeclass for lifting EventContainers to Catenable?
-- TODO: set convention for ordering events - is `head` first or last?

wrapCat :: (Reflex t) => Event t a -> Event t [a]
wrapCat = fmap pure

ecCons :: (Reflex t) => Event t a -> Event t [a] -> Event t [a]
ecCons evts = ecCat (wrapCat evts)

wrapConcat :: (Reflex t) => [Event t a] -> Event t [a]
wrapConcat = ecConcat . fmap wrapCat

bubbleWrapWith :: (Neverable s, Reflex t)
               => ASetter' s (Event t [a])
               -> Event t a
               -> Bubbling t s ()
bubbleWrapWith l = bubbleWith l . wrapCat
