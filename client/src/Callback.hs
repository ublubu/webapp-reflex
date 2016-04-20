module Callback where

import GHCJS.Types
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import GHCJS.Marshal

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef)
import Reflex.Dom

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Ref
import Data.Dependent.Map (DSum(..))

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
