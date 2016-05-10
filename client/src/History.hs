{-# LANGUAGE RecursiveDo #-}

module History where

import GHCJS.Types
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Marshal

import qualified Data.JSString as JSS
import JavaScript.Web.Location

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class

import Callback
import Utils

-- TODO: clean up the duplication in this file

foreign import javascript unsafe "history.pushState($1, '', $2)"
  historyPushState :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "window.onpopstate = function (x) { $1(x.state); }"
  setWindowOnpopstate :: Callback (JSVal -> IO ()) -> IO ()

historyWidget :: (MonadWidget t m, FromJSVal r, ToJSVal r)
              => (r -> String)
              -> Event t r -- pushstate events
              -> m (Event t r) -- popstate events
historyWidget toPath pushStates = mdo
  let push route = do
        jvRoute <- liftIO $ toJSVal route
        liftIO $ historyPushState jvRoute (JSS.pack $ toPath route)
  performEvent_ $ push <$> pushStates
  callbackEvent setWindowOnpopstate

pathnameHistoryWidget :: (MonadWidget t m, FromJSVal r, ToJSVal r)
                      => (String -> r)
                      -> (r -> String)
                      -> Event t r -- pushstate events
                      -> m (r, Event t r) -- (state0, popstate events)
pathnameHistoryWidget fromPath toPath pushStates = do
  location <- liftIO getWindowLocation
  path <- fmap JSS.unpack . liftIO $ getPathname location
  popStates <- historyWidget toPath pushStates
  return (fromPath path, popStates)

historyWidget' :: (MonadWidget t m, FromJSVal r, ToJSVal r)
               => (r -> String)
               -> Event t [r] -- pushstate events
               -> m (Event t r) -- popstate events
historyWidget' toPath pushStates = mdo
  let push route = do
        jvRoute <- liftIO $ toJSVal route
        liftIO $ historyPushState jvRoute (JSS.pack $ toPath route)
  performEvent_ $ mapM_ push <$> pushStates
  callbackEvent setWindowOnpopstate

pathnameHistoryWidget' :: (MonadWidget t m, FromJSVal r, ToJSVal r)
                       => (String -> r)
                       -> (r -> String)
                       -> Event t [r] -- pushstate events
                       -> m (r, Event t r) -- (state0, popstate events)
pathnameHistoryWidget' fromPath toPath pushStates = do
  location <- liftIO getWindowLocation
  path <- fmap JSS.unpack . liftIO $ getPathname location
  popStates <- historyWidget' toPath pushStates
  return (fromPath path, popStates)

-- NOTE: treats `head` of `[r]` as "least recent" (aka "first")
pathnameHistoryWidget'' :: (MonadWidget t m, FromJSVal r, ToJSVal r)
                        => (String -> r)
                        -> (r -> String)
                        -> Event t [r] -- pushstate events
                        -> m (Dynamic t r) -- current state
pathnameHistoryWidget'' fromPath toPath pushStates = do
  (state0, popStates) <- pathnameHistoryWidget' fromPath toPath pushStates
  holdDyn state0 $ leftmost [popStates, fmap last pushStates]
