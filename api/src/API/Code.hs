{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Code where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Int
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Utils.Links

import API.SignIn

data CodeEdit =
  CodeEdit { _ceTitle :: Text
           , _ceDescription :: Maybe Text
           , _ceCode :: Text
           } deriving (Show, Eq, Generic)
makeLenses ''CodeEdit

data CodeMeta =
  CodeMeta { _cmAuthor :: Text
           , _cmGuid :: Text
           } deriving (Show, Eq, Generic)

type CodeView = (CodeMeta, CodeEdit)

instance FromJSON CodeEdit
instance ToJSON CodeEdit

instance FromJSON CodeMeta
instance ToJSON CodeMeta

type Paged x = QueryParam "offset" Int :> QueryParam "count" Int :> x

type Crud keyName keyType edit view =
  ReqBody '[JSON] edit :> Cookied :> Post '[JSON] view
  :<|> Capture keyName keyType :> Get '[JSON] view
  :<|> Capture keyName keyType :> ReqBody '[JSON] edit :> Cookied :> Put '[JSON] view
  :<|> Capture keyName keyType :> Cookied :> Delete '[JSON] ()

type CodeAPI =
  "code" :> Paged (Get '[JSON] [CodeView])
  :<|> "code" :> Crud "guid" Text CodeEdit CodeView
  :<|> "myCode" :> Paged (Cookied :> Get '[JSON] [CodeView])
