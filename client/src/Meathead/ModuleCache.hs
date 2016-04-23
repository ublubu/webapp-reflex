{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Meathead.ModuleCache where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Lens.At
import Control.Lens.TH
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

import API.Module

import APIClient
import Apply
import Combinators
import Utils

import Meathead.APIClient
import Meathead.Pages

-- TODO: newtype wrapper for Text to specify GUID-ness

newtype RequestGuid =
  RequestGuid { _unRequestGuid :: Text
              } deriving (Show, Eq, Ord)
makeLenses ''RequestGuid

data ModuleRequest =
  CreateModule RequestGuid ModuleCreate
  | ReadModule Text
  | UpdateModule Text ModuleEdit
  | DeleteModule Text
  | ReadModules PagingState
  | ReadMyModules PagingState
makePrisms ''ModuleRequest

-- TODO: make a proper cache
data ModuleListCache =
  ModuleListCache { _mlcPaging :: PagingState
                  , _mlcPage :: [ModuleView]
                  } deriving (Show, Eq)
makeLenses ''ModuleListCache

-- TODO: make a proper cache
data ModuleCache =
  ModuleCache { _mcMap :: Map Text ModuleView
              , _mcAll :: ModuleListCache
              , _mcMine :: ModuleListCache
              , _mcCreated :: Map RequestGuid Text
              } deriving (Show, Eq)
makeLenses ''ModuleCache

data ModuleCacheUpdate =
  SetModuleUpdate ModuleView
  | CreateModuleUpdate RequestGuid ModuleView
  | ModifyModuleUpdate ModuleEditView
  | DeleteModuleUpdate Text
  | AllModulesUpdate PagingState [ModuleView]
  | MyModulesUpdate PagingState [ModuleView]
makePrisms ''ModuleCacheUpdate

-- analogous to flux store
moduleCache :: (MonadWidget t m)
            => ModuleCache
            -> Event t ModuleRequest
            -> m (Dynamic t ModuleCache)
moduleCache cache0 reqE = do
  resE <- performWithLog handleModuleRequest reqE
  foldDyn applyCacheUpdate cache0 resE

setModules :: Map Text ModuleView -> [ModuleView] -> Map Text ModuleView
setModules =
  foldl (\cacheMap module_ -> M.insert (module_ ^. _1.mmGuid) module_ cacheMap)

-- TODO: does SetModule update All/Mine?
-- TODO: ModifyModule should update All/Mine
applyCacheUpdate :: ModuleCacheUpdate -> ModuleCache -> ModuleCache
applyCacheUpdate (SetModuleUpdate module_) =
  mcMap %~ M.insert (module_ ^. _1.mmGuid) module_
applyCacheUpdate (CreateModuleUpdate reqGuid module_) =
  applyCacheUpdate (SetModuleUpdate module_)
  . (mcCreated %~ M.insert reqGuid (module_ ^. _1.mmGuid))
applyCacheUpdate (ModifyModuleUpdate (meta, edit)) =
  mcMap.(ix $ _mmGuid meta) %~ (\(_, parentGuid, _) -> (meta, parentGuid, edit))
applyCacheUpdate (DeleteModuleUpdate guid) =
  (mcMap %~ M.delete guid)
  . (mcAll %~ deleteGuidFromList guid)
  . (mcMine %~ deleteGuidFromList guid)
applyCacheUpdate (AllModulesUpdate paging modules) =
  (mcAll .~ ModuleListCache paging modules)
  . (mcMap %~ flip setModules modules)
applyCacheUpdate (MyModulesUpdate paging modules) =
  (mcMine .~ ModuleListCache paging modules)
  . (mcMap %~ flip setModules modules)

handleModuleRequest :: ModuleRequest -> ServIO ModuleCacheUpdate
handleModuleRequest (CreateModule reqGuid module_) =
  CreateModuleUpdate reqGuid <$> postModule module_
handleModuleRequest (ReadModule guid) =
  SetModuleUpdate <$> getModule guid
handleModuleRequest (UpdateModule guid module_) =
  ModifyModuleUpdate <$> putModule guid module_
handleModuleRequest (DeleteModule guid) =
  deleteModule guid >> return (DeleteModuleUpdate guid)

-- TODO: partial results?
maybeModulePage :: PagingState-> ModuleListCache -> Maybe [ModuleView]
maybeModulePage paging ModuleListCache{..} =
  if paging == _mlcPaging then Just _mlcPage
  else Nothing

-- TODO: make a proper cache
deleteGuidFromList :: Text -> ModuleListCache -> ModuleListCache
deleteGuidFromList guid cache =
  cache & mlcPage %~ filter ((== guid) . view (_1 . mmGuid))

moduleLookup :: Text -> ModuleCache -> Maybe ModuleView
moduleLookup guid = M.lookup guid . _mcMap

getReqGuid :: MonadIO m => m RequestGuid
getReqGuid = liftIO $ RequestGuid . U.toText <$> U.nextRandom

createdLookup :: RequestGuid -> ModuleCache -> Maybe Text
createdLookup reqGuid = M.lookup reqGuid . _mcCreated

-- fires once - when the module creation request is completed
moduleCreated :: forall t m. (MonadWidget t m)
              => RequestGuid
              -> Dynamic t ModuleCache
              -> m (Event t Text)
moduleCreated reqGuid cache = do
  guidD <- createdLookup reqGuid @/ cache
  dWithFirst guidD fire

emptyModuleListCache :: ModuleListCache
emptyModuleListCache = ModuleListCache (PagingState 0 0) []

emptyModuleCache :: ModuleCache
emptyModuleCache =
  ModuleCache M.empty emptyModuleListCache emptyModuleListCache M.empty
