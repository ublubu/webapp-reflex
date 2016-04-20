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

foreign import javascript unsafe "history.pushState($1, '', $2)"
  historyPushState :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "window.onpopstate = function (x) { $1(x.state); }"
  setWindowOnpopstate :: Callback (JSVal -> IO ()) -> IO ()

-- TODO: make a version that just stores a counter/simple-state in history.state
--         and manages full state history in haskell?
historyWidget :: (MonadWidget t m, FromJSVal r, ToJSVal r)
              => (Dynamic t r -> m (Event t r))
              -> (r -> String)
              -> r
              -> m ()
historyWidget f toPath route0 = mdo
  currentRoute <- holdDyn route0 $ leftmost [popStates, newStates]
  popStates <- callbackEvent setWindowOnpopstate
  newStates <- f currentRoute
  let push route = do
        jvRoute <- liftIO $ toJSVal route
        liftIO $ historyPushState jvRoute (JSS.pack $ toPath route)
  performEvent_ $ push <$> newStates

pathnameHistoryWidget :: (MonadWidget t m, FromJSVal r, ToJSVal r)
                      => (String -> r)
                      -> (r -> String)
                      -> (Dynamic t r -> m (Event t r))
                      -> m ()
pathnameHistoryWidget fromPath toPath f = do
  location <- liftIO getWindowLocation
  path <- fmap JSS.unpack . liftIO $ getPathname location
  liftIO $ print path
  newStates <- historyWidget f toPath $ fromPath path
  noContents
