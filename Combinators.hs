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

instance (EventContainer t m a) => EventContainer t m (Routing t r a) where
  ecJoin ecEvt =
    Routing <$> ecExtractWith ecJoin _rRoutes ecEvt <*> ecExtractWith ecJoin _rContents ecEvt
  ecSwitchPromptly ecDyn =
    Routing <$> ecExtractWith' ecSwitchPromptly _rRoutes ecDyn <*> ecExtractWith' ecSwitchPromptly _rContents ecDyn

instance (Reflex t, Leftmostable a) => Leftmostable (Routing t r a) where
  ecLeftmost ecList =
    Routing (ecExtractLeftmost _rRoutes ecList) (ecExtractLeftmost _rContents ecList)

eCombine :: (Reflex t) => Event t a -> Event t a -> Event t a
eCombine a b = leftmost [a, b]

data Routing t r a = Routing { _rRoutes :: Event t r
                             , _rContents :: a
                             }
makeLenses ''Routing

rJoin :: (Reflex t) => Routing t r (Routing t r a) -> Routing t r a
rJoin (Routing r1 (Routing r2 a)) = Routing (r1 `eCombine` r2) a

class (Reflex t) => RoutingContainer t r c where
  type RContents c :: *
  rcFactor :: c -> Routing t r (RContents c)

instance (Reflex t) => RoutingContainer t r (Routing t r a, Routing t r b) where
  type RContents (Routing t r a, Routing t r b) = (a, b)
  rcFactor (ra, rb) =
    Routing (view rRoutes ra `eCombine` view rRoutes rb) (ra ^. rContents, rb ^. rContents)

instance (Reflex t) => RoutingContainer t r [Routing t r a] where
  type RContents [Routing t r a] = [a]
  rcFactor ras = Routing (leftmost $ fmap _rRoutes ras) (fmap _rContents ras)

rdIf :: (EventContainer t m a, EventContainer t m b) => Dynamic t Bool -> m (Routing t r a) -> m (Routing t r b) -> m (Routing t r (a, b))
rdIf test true false = fmap rcFactor $ dIf test true false

neverRouting :: (Reflex t) => a -> Routing t r a
neverRouting = Routing never

onlyRouting :: Routing t r a -> Routing t r ()
onlyRouting = set rContents ()

routing :: (Reflex t) => Event t r -> Routing t r ()
routing = flip Routing ()
