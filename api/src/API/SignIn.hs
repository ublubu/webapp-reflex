{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.SignIn where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.API

data CookieData = CookieData { _cookieDataUserId :: Text } deriving (Show, Eq, Generic)

instance FromJSON CookieData
instance ToJSON CookieData

type SetCookied a = Headers '[Header "Set-Cookie" Text] a
type Cookied = Header "Cookie" Text
type SignInAPI = "tokensignin" :> QueryParam "idtoken" Text :> Get '[JSON] (SetCookied CookieData)
                 :<|> "cookiedata" :> Cookied :> Get '[JSON] CookieData
