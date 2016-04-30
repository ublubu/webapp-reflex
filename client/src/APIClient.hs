{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIClient where

import GHCJS.Types
import GHCJS.Marshal

import Reflex
import Reflex.Dom

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Either
import Data.Int
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client

import API.SignIn

-- TODO: SetCookie shouldn't require a FromByteString instance
import Data.ByteString.Conversion.From
import Web.Cookie

-- TODO: these are for the garbage (To|From)JSVal instances
import Data.Aeson
import qualified Data.JSString as JSS
import qualified Data.ByteString.Lazy.Char8 as BSC

type ServIO = EitherT ServantError IO

-- TODO: figure out why ghcjs-servant-client requires these
--         apparently extraneous (To|From)JSVal instances

foreign import javascript unsafe "console.log($1)"
  console_log :: JSVal -> IO ()

foreign import javascript unsafe "JSON.stringify($1)"
  json_stringify :: JSVal -> IO JSString

foreign import javascript unsafe "JSON.parse($1)"
  json_parse :: JSString -> IO JSVal

-- TODO: seriously, though. figure out something better than this
instance FromJSVal CookieData where
  fromJSVal = fmap (decode . BSC.pack . JSS.unpack) . json_stringify

-- TODO: also, maybe this?
instance FromByteString SetCookie where
  parser = parseSetCookie <$> parser

baseUrl :: Maybe BaseUrl
baseUrl = (Just $ BaseUrl Http "localhost" 8081)

signInApi :: Proxy ("api" :> SignInAPI)
signInApi = Proxy

tokensignin :: Maybe Text -> ServIO (SetCookied CookieData)
cookiedata' :: Maybe Text -> ServIO CookieData

cookiedata :: ServIO CookieData
cookiedata = cookiedata' Nothing

tokensignin :<|> cookiedata' = client signInApi baseUrl

performWithLog :: forall a b t m. (MonadWidget t m)
               => (a -> ServIO b)
               -> Event t a
               -> m (Event t b)
performWithLog f evts = do
  results <- performEvent (fmap toAction evts)
  return $ fmapMaybe id results
  where toAction :: a -> WidgetHost m (Maybe b)
        toAction x = liftIO . (smashError =<<) . runEitherT . f $ x
        smashError :: Either ServantError b -> IO (Maybe b)
        smashError = either log (return . Just)
        log :: ServantError -> IO (Maybe b)
        log err = do
          logError err
          return Nothing

logError :: ServantError -> IO ()
logError (FailureResponse status ctype body) = do
  print $ "FailureResponse " ++ show status ++ " " ++ show ctype
  console_log body
logError (DecodeFailure err ctype body) = do
  print $ "DecodeFailure " ++ err ++ " " ++ show ctype
  console_log body
logError (UnsupportedContentType ctype body) = do
  print $ "UnsupportedContentType " ++ show ctype
  console_log body
logError (InvalidContentTypeHeader hdr body) = do
  print $ "InvalidContentTypeHeader " ++ BSC.unpack hdr
  console_log body
