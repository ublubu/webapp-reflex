{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Combinators where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad

{-
Let's assume that all widgets return a static value or Event stream.
Widgets can't do anything until they're built, so the initial value of
  any Dynamic they return would use some initial value owned by the parent.
-}

class (MonadWidget t m) => EventContainer t m a where
  ecDyn :: Dynamic t (m a) -> m a
  ecDyn = ecJoin <=< dyn

  ecDyn' :: (x -> m a) -> Dynamic t x -> m a
  ecDyn' f state = ecDyn =<< mapDyn f state

  ecNever :: m a
  ecNever = ecJoin never

  ecExtractWith :: (Functor f) => (f a -> m a) -> (x -> a) -> f x -> m a
  ecExtractWith join focus = join . fmap focus

  ecJoin :: Event t a -> m a

  ecLeftmost :: [a] -> m a

dWhen :: (EventContainer t m a) => Dynamic t Bool -> m a -> m a
dWhen test widget =
  ecDyn' (\t -> if t then widget else ecNever) test

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
  ecLeftmost [trues, falses]

instance (MonadWidget t m) => EventContainer t m () where
  ecJoin = const $ return ()
  ecLeftmost = const $ return ()

instance (MonadWidget t m) => EventContainer t m (Event t a) where
  ecJoin = (return . switchPromptlyDyn) <=< holdDyn never
  ecLeftmost = return . leftmost

instance (EventContainer t m a, EventContainer t m b) => EventContainer t m (a, b) where
  ecJoin = ecTuple2 ecJoin ecJoin
  ecLeftmost = ecTuple2 ecLeftmost ecLeftmost

ecTuple2 :: (EventContainer t m a, EventContainer t m b, Functor f) => (f a -> m a) -> (f b -> m b) -> f (a, b) -> m (a, b)
ecTuple2 f g eventPairs = (,) <$> (ecExtractWith f) fst eventPairs <*> (ecExtractWith g) snd eventPairs

instance (EventContainer t m a) => EventContainer t m (Routing t r a) where
  ecJoin = ecRouting ecJoin ecJoin
  ecLeftmost = ecRouting ecLeftmost ecLeftmost

ecRouting :: (EventContainer t m a, Functor f) => (f (Event t r) -> m (Event t r)) -> (f a -> m a) -> f (Routing t r a) -> m (Routing t r a)
ecRouting f g routings = Routing <$> (ecExtractWith f) _rRoutes routings <*> (ecExtractWith g) _rContents routings

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
  rcFactor (ra, rb) = Routing (view rRoutes ra `eCombine` view rRoutes rb) (ra ^. rContents, rb ^. rContents)

rdIf :: (EventContainer t m a, EventContainer t m b) => Dynamic t Bool -> m (Routing t r a) -> m (Routing t r b) -> m (Routing t r (a, b))
rdIf test true false = fmap rcFactor $ dIf test true false

neverRouting :: (Reflex t) => a -> Routing t r a
neverRouting = Routing never
