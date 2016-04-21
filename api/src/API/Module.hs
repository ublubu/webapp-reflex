{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Module where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Int
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Utils.Links

import API.SignIn

data ModuleEdit =
  ModuleEdit { _meTitle :: Text
             , _meDescription :: Maybe Text
             , _meCode :: Text
             } deriving (Show, Eq, Generic)
makeLenses ''ModuleEdit

data ModuleMeta =
  ModuleMeta { _mmAuthor :: Text
             , _mmGuid :: Text
             } deriving (Show, Eq, Generic)

type ModuleView = (ModuleMeta, Maybe Text, ModuleEdit)
type ModuleCreate = (Maybe Text, ModuleEdit)

type ModuleEditView = (ModuleMeta, ModuleEdit)

instance FromJSON ModuleEdit
instance ToJSON ModuleEdit

instance FromJSON ModuleMeta
instance ToJSON ModuleMeta

type Paged x = QueryParam "offset" Int :> QueryParam "count" Int :> x

type Crud keyName keyType create view edit editView =
  ReqBody '[JSON] create :> Cookied :> Post '[JSON] view
  :<|> Capture keyName keyType :> Get '[JSON] view
  :<|> Capture keyName keyType :> ReqBody '[JSON] edit :> Cookied :> Put '[JSON] editView
  :<|> Capture keyName keyType :> Cookied :> Delete '[JSON] ()

type ModuleAPI =
  "modules" :> Paged (Get '[JSON] [ModuleView])
  :<|> "module" :> Crud "guid" Text ModuleCreate ModuleView ModuleEdit ModuleEditView
  :<|> "myModules" :> Paged (Cookied :> Get '[JSON] [ModuleView])
