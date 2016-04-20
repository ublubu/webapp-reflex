{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module APIClient where

import GHCJS.Marshal

import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Int
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client

import API.SignIn
import API.Code

type ServIO = EitherT ServantError IO

-- TODO: figure out why ghcjs-servant-client requires these
--         apparently extraneous (To|From)JSVal instances

instance FromJSVal CookieData where
  fromJSVal = (fmap . fmap) CookieData . fromJSVal

baseUrl :: Maybe BaseUrl
baseUrl = (Just $ BaseUrl Http "localhost" 8081)

signInApi :: Proxy ("api" :> SignInAPI)
signInApi = Proxy

tokensignin :: Maybe Text -> ServIO (SetCookied CookieData)
cookiedata' :: Maybe Text -> ServIO CookieData

cookiedata :: ServIO CookieData
cookiedata = cookiedata' Nothing

tokensignin :<|> cookiedata' = client signInApi baseUrl

codeApi :: Proxy ("api" :> CodeAPI)
codeApi = Proxy

instance ToJSVal CodeEdit where
  toJSVal CodeEdit{..} =
    toJSVal (_ceTitle, _ceDescription, _ceCode)

instance FromJSVal CodeEdit where
  fromJSVal jv = runMaybeT $ do
    (title, description, code) <- MaybeT $ fromJSVal jv
    return $ CodeEdit { _ceTitle = title
                      , _ceDescription = description
                      , _ceCode = code
                      }

instance FromJSVal CodeMeta where
  fromJSVal jv = runMaybeT $ do
    (author, guid) <- MaybeT $ fromJSVal jv
    return $ CodeMeta { _cmAuthor = author
                      , _cmGuid = guid
                      }

getCodes :: Maybe Int -> Maybe Int -> ServIO [CodeView]
postCode' :: CodeEdit -> Maybe Text -> ServIO CodeView
getCode :: Text -> ServIO CodeView
putCode' :: Text -> CodeEdit -> Maybe Text -> ServIO CodeView
deleteCode' :: Text -> Maybe Text -> ServIO ()
myCodes' :: Maybe Int -> Maybe Int -> Maybe Text -> ServIO [CodeView]

getCodes :<|> (postCode' :<|> getCode :<|> putCode' :<|> deleteCode') :<|> myCodes' =
  client codeApi baseUrl

postCode :: CodeEdit -> ServIO CodeView
postCode = flip postCode' Nothing

putCode :: Text -> CodeEdit -> ServIO CodeView
putCode k v = putCode' k v Nothing

deleteCode :: Text -> ServIO ()
deleteCode = flip deleteCode' Nothing

myCodes :: Maybe Int -> Maybe Int -> ServIO [CodeView]
myCodes offset count = myCodes' offset count Nothing
