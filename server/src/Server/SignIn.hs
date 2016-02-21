{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.SignIn where

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Monoid
import Data.Text (Text)
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client.Conduit as H
import qualified Network.HTTP.Types.Status as H
import Servant
import Servant.Server
import Web.ClientSession

import API.SignIn
import Server.App

signInServer :: ServerT SignInAPI App
signInServer =
  tokensignin :<|> cookiedata

  where tokensignin :: Maybe Text -> App (SetCookied CookieData)
        tokensignin token = case token of
          Nothing -> throwError $ Invalid "missing token in queryparams"
          Just t -> askGoogle t

        cookiedata :: Maybe Text -> App CookieData
        cookiedata = withCookieText return

data GoogleTokenInfo = GoogleTokenInfo { _googleTokenAud :: Text
                                       , _googleTokenSub :: Text
                                       } deriving (Show, Eq)

instance FromJSON GoogleTokenInfo where
  parseJSON (Object v) = GoogleTokenInfo <$> v .: "aud" <*> v .: "sub"
  parseJSON _ = mzero

withCookieText :: (CookieData -> App a) -> Maybe Text -> App a
withCookieText f =
  f <=< maybe (throwError $ Invalid "no cookie provided") (decryptCookie . unwrapCookie)

cookiePrefix :: Text
cookiePrefix = "Session="

wrapCookie :: Text -> Text
wrapCookie cookie = cookiePrefix <> cookie

unwrapCookie :: Text -> Text
unwrapCookie = T.drop (T.length cookiePrefix)

getCookieData' :: GoogleTokenInfo -> CookieData
getCookieData' = CookieData . _googleTokenSub

getCookieData :: GoogleTokenInfo -> App CookieData
getCookieData = return . getCookieData'

encryptCookie :: CookieData -> App Text
encryptCookie cookie = do
  iv <- liftIO randomIV
  config <- ask
  return . T.decodeUtf8 $ encrypt (_appConfigClientSessionKey config) iv (BSL.toStrict $ encode cookie)

decryptCookie :: Text -> App CookieData
decryptCookie cookieText = do
  config <- ask
  let key = _appConfigClientSessionKey config
      cookieBytes = T.encodeUtf8 cookieText
  cookie <- maybe (throwError $ Invalid "couldn't decrypt cookie") return (decrypt key cookieBytes)
  maybe (throwError $ Invalid "couldn't parse cookie") return . decodeStrict' $ cookie

askGoogle :: Text -> App (SetCookied CookieData)
askGoogle token = do
  request <- liftIO $ H.parseUrl "https://www.googleapis.com/oauth2/v3/tokeninfo"
  let r = H.setQueryString [("id_token", Just $ T.encodeUtf8 token)] request
  tokenInfo <- H.withResponse r processGoogleResponse
  cookieData <- getCookieData tokenInfo
  cookie <- encryptCookie cookieData
  return $ addHeader (wrapCookie cookie) cookieData

processGoogleResponse :: H.Response (ConduitM () BS.ByteString App ()) -> App GoogleTokenInfo
processGoogleResponse res = do
  let bodyProducer = H.responseBody res
  body <- runConduit $ bodyProducer =$= C.fold mappend mempty
  when (H.responseStatus res /= H.status200) $ throwError
    $ Invalid "google says your token is no good"
  let tokenInfo = (decodeStrict' body :: Maybe GoogleTokenInfo)
  case tokenInfo of
    Nothing -> throwError $ Invalid "google's response wasn't token info"
    Just t -> do
      config <- ask
      when (_appConfigGoogleClientId config /= _googleTokenAud t)
        $ throwError $ Invalid "your token isn't for this app"
      return t
