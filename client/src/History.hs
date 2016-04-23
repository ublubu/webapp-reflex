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

historyWidget_ :: (MonadWidget t m, FromJSVal r, ToJSVal r)
               => (r -> String)
               -> Event t r -- pushstate events
               -> m (Event t r) -- popstate events
historyWidget_ toPath pushStates = mdo
  let push route = do
        jvRoute <- liftIO $ toJSVal route
        liftIO $ historyPushState jvRoute (JSS.pack $ toPath route)
  performEvent_ $ push <$> pushStates
  callbackEvent setWindowOnpopstate

pathnameHistoryWidget_ :: (MonadWidget t m, FromJSVal r, ToJSVal r)
                       => (String -> r)
                       -> (r -> String)
                       -> Event t r -- pushstate events
                       -> m (r, Event t r) -- (state0, popstate events)
pathnameHistoryWidget_ fromPath toPath pushStates = do
  location <- liftIO getWindowLocation
  path <- fmap JSS.unpack . liftIO $ getPathname location
  popStates <- historyWidget_ toPath pushStates
  return (fromPath path, popStates)

unitHistoryWidget :: (MonadWidget t m)
                  => (String -> r)
                  -> (r -> String)
                  -> Event t r -- pushstate events
                  -> m (Dynamic t r) -- current state
unitHistoryWidget fromPath toPath pushStates = do
  location <- liftIO getWindowLocation
  path <- fmap JSS.unpack . liftIO $ getPathname location
  popUnits <- historyWidget_ id (toPath <$> pushStates)
  let updates = leftmost [fmap (:) pushStates, eventValue tail popUnits]
      state0 = fromPath path
  historyD <- foldDyn ($) [state0] updates
  mapDyn head historyD
