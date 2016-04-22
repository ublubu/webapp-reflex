{-# LANGUAGE TemplateHaskell #-}

module ModuleCache where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Lens.At
import Control.Lens.TH
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M

import API.Module

import APIClient
import Meathead.APIClient
import Meathead.Pages

-- TODO: newtype wrapper for Text to specify GUID-ness

data ModuleRequest =
  CreateModule ModuleCreate
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
              } deriving (Show, Eq)
makeLenses ''ModuleCache

data ModuleCacheUpdate =
  SetModuleUpdate ModuleView
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

applyCacheUpdate :: ModuleCacheUpdate -> ModuleCache -> ModuleCache
applyCacheUpdate (SetModuleUpdate module_) =
  mcMap %~ M.insert (module_ ^. _1.mmGuid) module_
applyCacheUpdate (ModifyModuleUpdate (meta, edit)) =
  mcMap.(ix $ _mmGuid meta) %~ (\(_, parentGuid, _) -> (meta, parentGuid, edit))
applyCacheUpdate (DeleteModuleUpdate guid) =
  mcMap %~ M.delete guid
applyCacheUpdate (AllModulesUpdate paging modules) =
  (mcAll .~ ModuleListCache paging modules)
  . (mcMap %~ flip setModules modules)
applyCacheUpdate (MyModulesUpdate paging modules) =
  (mcMine .~ ModuleListCache paging modules)
  . (mcMap %~ flip setModules modules)

handleModuleRequest :: ModuleRequest -> ServIO ModuleCacheUpdate
handleModuleRequest (CreateModule module_) =
  SetModuleUpdate <$> postModule module_
handleModuleRequest (ReadModule guid) =
  SetModuleUpdate <$> getModule guid
handleModuleRequest (UpdateModule guid module_) =
  ModifyModuleUpdate <$> putModule guid module_
handleModuleRequest (DeleteModule guid) =
  deleteModule guid >> return (DeleteModuleUpdate guid)
