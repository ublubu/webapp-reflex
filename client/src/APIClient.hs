{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module APIClient where

import GHCJS.Types
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

-- TODO: seriously, though. figure out something better than this
instance FromJSVal CodeEdit where
  fromJSVal = fmap (decode . BSC.pack . JSS.unpack) . json_stringify

instance FromJSVal CodeMeta where
  fromJSVal = fmap (decode . BSC.pack . JSS.unpack) . json_stringify

instance ToJSVal CodeEdit where
  toJSVal = json_parse . JSS.pack . BSC.unpack . encode

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
