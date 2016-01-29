{-# LANGUAGE RecursiveDo #-}

module Utils where

import Reflex
import Reflex.Dom

import Control.Monad
import Data.Either.Combinators
import qualified Data.Foldable as F
import Data.Sequence (Seq)

noContents :: (MonadWidget t m) => m ()
noContents = return ()

dynWidgetDyn :: (MonadWidget t m) => a -> Dynamic t (m (Dynamic t a)) -> m (Dynamic t a)
dynWidgetDyn val = fmap joinDyn . (holdDyn (constDyn val) =<<) . dyn

dynWidgetDyn' :: (MonadWidget t m) => b -> (a -> m (Dynamic t b)) -> Dynamic t a -> m (Dynamic t b)
dynWidgetDyn' val f state = dynWidgetDyn val =<< mapDyn f state

dynWidgetEvents :: (MonadWidget t m) => Dynamic t (m (Event t a)) -> m (Event t a)
dynWidgetEvents = fmap switchPromptlyDyn . (holdDyn never =<<) . dyn

dynWidgetEvents' :: (MonadWidget t m) => (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
dynWidgetEvents' f state = dynWidgetEvents =<< mapDyn f state

dynWidgetEvents'' :: (MonadWidget t m) => Dynamic t (m (Dynamic t a)) -> m (Event t a)
dynWidgetEvents'' = fmap updated . dynWidgetDyn undefined

dynWidgetEvents''' :: (MonadWidget t m) => (a -> m (Dynamic t b)) -> Dynamic t a -> m (Event t b)
dynWidgetEvents''' f state = dynWidgetEvents'' =<< mapDyn f state

eventJoin :: (MonadWidget t m) => Event t (Event t a) -> m (Event t a)
eventJoin = (return . switchPromptlyDyn) <=< holdDyn never

eventDynJoin :: (MonadWidget t m) => Event t (Dynamic t a) -> m (Event t a)
eventDynJoin = fmap (updated . joinDyn) . holdDyn (constDyn undefined)

widgetFromEvent :: (MonadWidget t m) => m a -> (b -> m a) -> Event t b -> m (Event t a)
widgetFromEvent init makeWidget evt =
  dyn =<< holdDyn init (fmap makeWidget evt)

untilFirstEvent :: (MonadWidget t m) => m (Event t a) -> m (Event t a)
untilFirstEvent init = mdo
  evt <- eventJoin =<< dyn =<< holdDyn init (fmap (const $ return never) evt)
  return evt

afterFirstEvent :: (MonadWidget t m) => m b -> (a -> m b) -> Event t a -> m (Event t b)
afterFirstEvent init makeWidget evt =
  dyn =<< holdDyn init (fmap makeWidget evt)

widgetSequence' :: (MonadWidget t m) => (a -> b) -> m (Event t a) -> (a -> m (Event t b)) -> m (Event t b)
widgetSequence' convert init after = do
  (evt, evt') <- widgetSequence init after
  return $ leftmost [fmap convert evt, evt']

widgetSequence :: (MonadWidget t m) => m (Event t a) -> (a -> m (Event t b)) -> m (Event t a, Event t b)
widgetSequence init after = do
  evt <- untilFirstEvent init
  evt' <- eventJoin =<< afterFirstEvent (return never) after evt
  return (evt, evt')

seqLeftmost :: (Reflex t) => Seq (Event t a) -> Event t a
seqLeftmost = leftmost . F.toList

modEvent :: (Reflex t) => (a -> b) -> Dynamic t a -> Event t x -> Event t b
modEvent f val trigger =
  fmap f $ tagDyn val trigger

modEvent' :: (Reflex t) => (a -> b) -> Dynamic t a -> Event t x -> Event t b
modEvent' f val trigger =
  fmap f $ tag (current val) trigger

mapEither :: (b -> a) -> (c -> a) -> Either b c -> a
mapEither f g = fromEither . mapBoth f g

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

dap :: (MonadWidget t m) => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
f `dap` x = f >>= combineDyn (flip ($)) x

dwhen' :: (MonadWidget t m) => Dynamic t Bool -> m (Dynamic t a) -> m (Event t a)
dwhen' test widget = do
  let widget' = mapDyn Just =<< widget
  evts <- dynWidgetEvents''' (\t -> if t then widget' else return $ constDyn Nothing) test
  return $ fmapMaybe id evts

ewhen' :: (MonadWidget t m) => Event t a -> m (Dynamic t b) -> m (Event t b)
ewhen' test widget = do
  test' <- holdDyn False (fmap (const True) test)
  dwhen' test' widget
