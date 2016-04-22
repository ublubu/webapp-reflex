{-# LANGUAGE TemplateHaskell #-}
module Meathead.Pages where

import Control.Lens.TH
import API.Module

data PagingState =
  PagingState { _pagingOffset :: Int
              , _pagingCount :: Int
              } deriving (Show, Eq)
makeLenses ''PagingState

data AllState =
  AllState { _allPaging :: PagingState
           } deriving (Show, Eq)
makeLenses ''AllState

data MyState =
  MyState { _myPaging :: PagingState
          } deriving (Show, Eq)
makeLenses ''MyState

data ModuleState =
  ModuleState { _mpMeta :: ModuleMeta
              , _mpEditing :: Bool
              } deriving (Show, Eq)
makeLenses ''ModuleState

data PageState =
  AllPage AllState | MyPage MyState | ModulePage ModuleState
makePrisms ''PageState
