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
import Data.Foldable (find)
import Data.Monoid
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Conversion.To
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client.Conduit as H
import qualified Network.HTTP.Types.Status as H
import Servant
import Servant.Server
import Web.ClientSession
import Web.Cookie

import API.SignIn
import Server.App

instance ToByteString SetCookie where
  builder = renderSetCookie

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
withCookieText f mCookies =
  case mCookies of
    Nothing -> throwError $ Invalid "no cookies provided"
    Just cookies -> f =<< maybe (throwError $ Invalid "couldn't unwrap cookie") decryptCookie (unwrapCookie cookies)

withCookieText1 :: (x -> CookieData -> App a) -> x -> Maybe Text -> App a
withCookieText1 f = withCookieText . f

withCookieText2 :: (x -> y -> CookieData -> App a) -> x -> y -> Maybe Text -> App a
withCookieText2 f = withCookieText1 . f

wrapCookie :: Text -> SetCookie
wrapCookie x = def { setCookieName = "Session", setCookieValue = T.encodeUtf8 x }

unwrapCookie :: Text -> Maybe Text
unwrapCookie cookies =
  snd <$> find ((== "Session") . fst) pairs
  where pairs = parseCookiesText . T.encodeUtf8 $ cookies

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
