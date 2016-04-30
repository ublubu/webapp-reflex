{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.SignIn where

import GHC.Generics

import Control.Monad
import Data.Aeson
import Data.Text (Text)

import Servant.API
import Web.Cookie

data CookieData = CookieData { _cookieDataUserId :: Text } deriving (Show, Eq, Generic)

instance FromJSON CookieData
instance ToJSON CookieData

-- TODO: allow client to read /some/ cookies without asking server?
--       e.g. using `CookiesText` (from "cookie" package) for `Cookied`
type SetCookied a = Headers '[Header "Set-Cookie" SetCookie] a
type Cookied = Header "Cookie" Text
type SignInAPI =
  "tokensignin" :> QueryParam "idtoken" Text :> Get '[JSON] (SetCookied CookieData)
  :<|> "cookiedata" :> Cookied :> Get '[JSON] CookieData
