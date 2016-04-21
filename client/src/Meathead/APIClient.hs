{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Meathead.APIClient where

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

import API.Module
import APIClient

-- TODO: these are for the garbage (To|From)JSVal instances
import Data.Aeson
import qualified Data.JSString as JSS
import qualified Data.ByteString.Lazy.Char8 as BSC

codeApi :: Proxy ("api" :> ModuleAPI)
codeApi = Proxy

-- TODO: seriously, though. figure out something better than this
instance FromJSVal ModuleEdit where
  fromJSVal = fmap (decode . BSC.pack . JSS.unpack) . json_stringify

instance FromJSVal ModuleMeta where
  fromJSVal = fmap (decode . BSC.pack . JSS.unpack) . json_stringify

instance ToJSVal ModuleEdit where
  toJSVal = json_parse . JSS.pack . BSC.unpack . encode

getModules :: Maybe Int -> Maybe Int -> ServIO [ModuleView]
postModule' :: ModuleCreate -> Maybe Text -> ServIO ModuleView
getModule :: Text -> ServIO ModuleView
putModule' :: Text -> ModuleEdit -> Maybe Text -> ServIO ModuleEditView
deleteModule' :: Text -> Maybe Text -> ServIO ()
myModules' :: Maybe Int -> Maybe Int -> Maybe Text -> ServIO [ModuleView]

getModules :<|> (postModule' :<|> getModule :<|> putModule' :<|> deleteModule') :<|> myModules' =
  client codeApi baseUrl

postModule :: ModuleCreate -> ServIO ModuleView
postModule = flip postModule' Nothing

putModule :: Text -> ModuleEdit -> ServIO ModuleEditView
putModule k v = putModule' k v Nothing

deleteModule :: Text -> ServIO ()
deleteModule = flip deleteModule' Nothing

myModules :: Maybe Int -> Maybe Int -> ServIO [ModuleView]
myModules offset count = myModules' offset count Nothing
