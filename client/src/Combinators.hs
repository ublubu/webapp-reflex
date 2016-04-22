{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Combinators where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Maybe

{-
Let's assume that all widgets return a static value or Event stream.
Widgets can't do anything until they're built, so the initial value of
  any Dynamic they return would use some initial value owned by the parent.
-}

type ExtractWith f x m a = (f a -> m a) -> (x -> a) -> f x -> m a

class Leftmostable a where
  ecExtractLeftmost :: (x -> a) -> [x] -> a
  ecExtractLeftmost focus = ecLeftmost . fmap focus

  ecLeftmost :: [a] -> a

class (Leftmostable a, MonadWidget t m) => EventContainer t m a where
  ecDyn :: Dynamic t (m a) -> m a
  ecDyn = ecJoin <=< dyn

  ecDyn' :: (x -> m a) -> Dynamic t x -> m a
  ecDyn' f state = ecDyn =<< mapDyn f state

  ecNever :: m a
  ecNever = ecJoin never

  ecExtractWith :: (Functor f) => ExtractWith f x m a
  ecExtractWith join focus = join . fmap focus

  ecExtractWith' :: ExtractWith (Dynamic t) x m a
  ecExtractWith' join focus = join <=< mapDyn focus

  ecSwitchPromptly :: Dynamic t a -> m a

  ecJoin :: Event t a -> m a

dWhen :: (EventContainer t m a) => Dynamic t Bool -> m a -> m a
dWhen test widget =
  ecDyn' (\t -> if t then widget else ecNever) test

dWhenJust :: (EventContainer t m a) => Dynamic t (Maybe x) -> (Dynamic t x -> m a) -> m a
dWhenJust maybeVal makeWidget = do
  val <- mapDyn maybeToList maybeVal
  evtsDyn <- mapDyn ecLeftmost =<< simpleList val makeWidget
  ecSwitchPromptly evtsDyn

eWhen :: (EventContainer t m a) => Event t b -> m a -> m a
eWhen test widget = do
  test' <- holdDyn False (fmap (const True) test)
  dWhen test' widget

dIf :: (EventContainer t m a, EventContainer t m b) => Dynamic t Bool -> m a -> m b -> m (a, b)
dIf test true false = do
  trues <- dWhen test true
  notTest <- mapDyn not test
  falses <- dWhen notTest false
  return (trues, falses)

dIf' :: (EventContainer t m a) => Dynamic t Bool -> m a -> m a -> m a
dIf' test true false = do
  (trues, falses) <- dIf test true false
  return $ ecLeftmost [trues, falses]

instance (MonadWidget t m) => EventContainer t m () where
  ecJoin = const $ return ()
  ecSwitchPromptly = const $ return ()

instance Leftmostable () where
  ecLeftmost = const ()

instance (MonadWidget t m) => EventContainer t m (Event t a) where
  ecJoin = switchPromptly never
  ecSwitchPromptly = return . switchPromptlyDyn

instance (Reflex t) => Leftmostable (Event t a) where
  ecLeftmost = leftmost

instance (EventContainer t m a, EventContainer t m b) => EventContainer t m (a, b) where
  ecJoin ecEvt =
    (,) <$> ecExtractWith ecJoin fst ecEvt <*> ecExtractWith ecJoin snd ecEvt
  ecSwitchPromptly ecDyn =
    (,) <$> ecExtractWith' ecSwitchPromptly fst ecDyn <*> ecExtractWith' ecSwitchPromptly snd ecDyn

instance (Leftmostable a, Leftmostable b) => Leftmostable (a, b) where
  ecLeftmost ecList =
    (ecExtractLeftmost fst ecList, ecExtractLeftmost snd ecList)

instance (EventContainer t m a, EventContainer t m r) => EventContainer t m (Bubbling t r a) where
  ecJoin ecEvt =
    Bubbling <$> ecExtractWith ecJoin _bBubble ecEvt <*> ecExtractWith ecJoin _bContents ecEvt
  ecSwitchPromptly ecDyn =
    Bubbling <$> ecExtractWith' ecSwitchPromptly _bBubble ecDyn <*> ecExtractWith' ecSwitchPromptly _bContents ecDyn

instance (Reflex t, Leftmostable a, Leftmostable r) => Leftmostable (Bubbling t r a) where
  ecLeftmost ecList =
    Bubbling (ecExtractLeftmost _bBubble ecList) (ecExtractLeftmost _bContents ecList)

ecCombine :: (Leftmostable a) => a -> a -> a
ecCombine a b = ecLeftmost [a, b]

data Bubbling t r a =
  Bubbling { _bBubble :: r
           , _bContents :: a
           }
makeLenses ''Bubbling

rJoin :: (Reflex t, Leftmostable r) => Bubbling t r (Bubbling t r a) -> Bubbling t r a
rJoin (Bubbling r1 (Bubbling r2 a)) = Bubbling (r1 `ecCombine` r2) a

class (Reflex t) => BubblingContainer t r c where
  type RContents c :: *
  rcFactor :: c -> Bubbling t r (RContents c)

instance (Reflex t, Leftmostable r) => BubblingContainer t r (Bubbling t r a, Bubbling t r b) where
  type RContents (Bubbling t r a, Bubbling t r b) = (a, b)
  rcFactor (ra, rb) =
    Bubbling (view bBubble ra `ecCombine` view bBubble rb) (ra ^. bContents, rb ^. bContents)

instance (Reflex t, Leftmostable r) => BubblingContainer t r [Bubbling t r a] where
  type RContents [Bubbling t r a] = [a]
  rcFactor ras = Bubbling (ecLeftmost $ fmap _bBubble ras) (fmap _bContents ras)

-- TODO: typeclass for automatically lifting a sub-r into r
--       e.g. `r` is giant sum type of various global actions (like flux)
rdIf :: (EventContainer t m r, EventContainer t m a, EventContainer t m b) => Dynamic t Bool -> m (Bubbling t r a) -> m (Bubbling t r b) -> m (Bubbling t r (a, b))
rdIf test true false = fmap rcFactor $ dIf test true false

neverBubbling :: (EventContainer t m r) => a -> m (Bubbling t r a)
neverBubbling x = flip Bubbling x <$> ecNever

onlyBubbling :: Bubbling t r a -> Bubbling t r ()
onlyBubbling = set bContents ()

bubbling :: (Reflex t) => r -> Bubbling t r ()
bubbling = flip Bubbling ()
