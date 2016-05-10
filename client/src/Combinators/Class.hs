{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Combinators.Class where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Foldable (foldl')
import Data.Maybe
import Data.Semigroup

import Apply

{-
Let's assume that all widgets return unit or an Event stream.
Widgets can't do anything until they're built, so any constant
  (including the initial value of a Dynamic)
  can be owned by the parent.

EventContainer is a container of Event streams.
-}

class Neverable a where
  ecNever :: a

class Neverable a => Catenable a where
  ecCat :: a -> a -> a
  ecConcat :: [a] -> a
  ecConcat = foldl' ecCat ecNever

class Leftmostable a where
  ecEither :: a -> a -> a
  ecEither x y = ecLeftmost [x, y]

  ecLeftmost :: [a] -> a

class (Reflex t) => Switchable t a where
  ecSwitch :: Behavior t a -> a
  ecSwitchPromptly :: (MonadWidget t m) => a -> Event t a -> m a

  -- TODO: This changes when Dynamic gets a Functor instance.
  ecSwitchPromptlyDyn :: (MonadWidget t m) => Dynamic t a -> m a

class (Neverable a, Switchable t a, Leftmostable a, MonadWidget t m) => EventContainer t m a where
  ecJoin :: Event t a -> m a

type EventContainer' t m a = (Catenable a, EventContainer t m a)

ecExtractWith :: (Functor f)
              => (f a -> m a)
              -> (x -> a)
              -> f x
              -> m a
ecExtractWith join focus = join . fmap focus

ecExtractWith' :: (Functor f)
               => (a -> f a -> m a)
               -> (x -> a)
               -> x
               -> f x
               -> m a
ecExtractWith' join focus x0 xUpdates =
  join a0 aUpdates
  where a0 = focus x0
        aUpdates = focus <$> xUpdates

ecExtractWith'' :: (MonadWidget t m)
                => (Dynamic t a -> m a)
                -> (x -> a)
                -> Dynamic t x
                -> m a
ecExtractWith'' join focus = join <=< mapDyn focus

instance Neverable () where
  ecNever = ()

instance Catenable () where
  ecCat _ _ = ()

instance Leftmostable () where
  ecLeftmost _ = ()

instance (Reflex t) => Switchable t () where
  ecSwitch _ = ()
  ecSwitchPromptly _ _ = return ()
  ecSwitchPromptlyDyn _ = return ()

instance (MonadWidget t m) => EventContainer t m () where
  ecJoin _ = return ()

instance (Reflex t) => Neverable (Event t a) where
  ecNever = never

instance (Reflex t, Semigroup a) => Catenable (Event t a) where
  ecCat a b = a `mappend` b

instance (Reflex t) => Leftmostable (Event t a) where
  ecLeftmost = leftmost

instance (Reflex t) => Switchable t (Event t a) where
  ecSwitch = switch
  ecSwitchPromptly = switchPromptly
  ecSwitchPromptlyDyn = return . switchPromptlyDyn

instance (MonadWidget t m) => EventContainer t m (Event t a) where
  ecJoin evts = ecSwitch <$> hold ecNever evts

instance (Neverable a, Neverable b) => Neverable (a, b) where
  ecNever = (ecNever, ecNever)

instance (Catenable a, Catenable b) => Catenable (a, b) where
  ecCat (a0, b0) (a1, b1) = (a0 `ecCat` a1, b0 `ecCat` b1)

instance (Leftmostable a, Leftmostable b) => Leftmostable (a, b) where
  ecLeftmost evts = (ecLeftmost $ fst <$> evts, ecLeftmost $ snd <$> evts)

instance (Switchable t a, Switchable t b) => Switchable t (a, b) where
  ecSwitch beh = (ecSwitch . fmap fst $ beh, ecSwitch . fmap snd $ beh)
  ecSwitchPromptly e0 eUpdates =
    (,) <$> ecSwitchPromptly (fst e0) (fmap fst eUpdates)
    <*> ecSwitchPromptly (snd e0) (fmap snd eUpdates)
  ecSwitchPromptlyDyn dVal =
    (,) <$> (ecSwitchPromptlyDyn =<< mapDyn fst dVal)
    <*> (ecSwitchPromptlyDyn =<< mapDyn snd dVal)

instance (EventContainer t m a, EventContainer t m b) => EventContainer t m (a, b) where
  ecJoin dVal =
    (,) <$> (ecJoin $ fst <$> dVal) <*> (ecJoin $ snd <$> dVal)
