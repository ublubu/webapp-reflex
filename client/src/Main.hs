{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHCJS.Types
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import GHCJS.Marshal

import qualified Data.JSString as JSS
import JavaScript.Web.Location

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef)
import Reflex.Dom

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Ref
import Data.Dependent.Map (DSum(..))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Monoid
import Data.Text (unpack)

import Combinators
import CommonWidgets
import Pages
import Style
import qualified Styles as S
import Utils

import API.SignIn

foreign import javascript unsafe "history.pushState($1, '', $2)"
  historyPushState :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "window.onpopstate = function (x) { $1(x.state); }"
  setWindowOnpopstate :: Callback (JSVal -> IO ()) -> IO ()

callbackEvent' :: (MonadWidget t m)
               => (Callback (JSVal -> IO ()) -> IO ())
               -> m (Event t JSVal)
callbackEvent' registerCallback = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  (eSignIn, eSignInTriggerRef) <- newEventWithTriggerRef
  let onAction :: JSVal -> IO ()
      onAction jsval = postGui $ do
        mt <- readRef eSignInTriggerRef
        forM_ mt $ \t -> runWithActions [t :=> pure jsval]
      setOnAction =
        registerCallback =<< (syncCallback1 ContinueAsync onAction)
  schedulePostBuild . liftIO $ setOnAction
  return eSignIn

callbackEvent :: (MonadWidget t m, FromJSVal r)
               => (Callback (JSVal -> IO ()) -> IO ())
               -> m (Event t r)
callbackEvent = fromJSValEvent <=< callbackEvent'

fromJSValEvent :: (MonadWidget t m, FromJSVal r)
               => Event t JSVal
               -> m (Event t r)
fromJSValEvent jsval = do
  let actions = fmap (runMaybeT . MaybeT . liftIO . fromJSVal) jsval
  mval <- performEvent actions
  return $ fmapMaybe id mval

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

main :: IO ()
main = mainWidget $ pathnameHistoryWidget parsePath (\i -> "/" ++ show i) dummyCounter
  where parsePath "/" = 0
        parsePath ('/':x) = read x
        parsePath _ = error "invalid path"

dummyCounter :: (MonadWidget t m)
             => Dynamic t Int
             -> m (Event t Int)
dummyCounter route = do
  btnPresses <- ecDyn' (button . show) route
  nextRoute <- mapDyn (+1) route
  return $ tag (current nextRoute) btnPresses
