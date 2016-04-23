{-# LANGUAGE TemplateHaskell #-}

module Meathead.Pages where

import Control.Lens.TH
import Data.Text (Text)

import API.Module

-- TODO: use String instead of Text since GHCJS/Reflex more String-friendly?
-- TODO: error page

data PagingState =
  PagingState { _pagingOffset :: Int
              , _pagingCount :: Int
              } deriving (Show, Eq)
makeLenses ''PagingState

-- TODO: newtype for guid
data ModulePageState =
  ModulePageState { _mpGuid :: Text
                  , _mpEditing :: Bool
                  } deriving (Show, Eq)
makeLenses ''ModulePageState

data PageState =
  AllModulesPage PagingState
  | MyModulesPage PagingState
  | ModulePage ModulePageState
  | ModuleCreatePage (Maybe Text) -- parent guid
  deriving (Show, Eq)
makePrisms ''PageState

pageViewModule :: Text -> PageState
pageViewModule guid =
  ModulePage $ ModulePageState { _mpGuid = guid
                               , _mpEditing = False
                               }

pageEditModule :: Text -> PageState
pageEditModule guid =
  ModulePage $ ModulePageState { _mpGuid = guid
                               , _mpEditing = True
                               }

-- TODO: finish or delete this
{-
instance FromJSVal PagingState where
  fromJSVal jv = fmap (uncurry PagingState) <$> fromJSVal jv

instance ToJSVal PagingState where
  toJSVal (PagingState a b) = toJSVal (a, b)

instance FromJSVal ModulePageState where
  fromJSVal jv = fmap (\(a, b) -> ModulePageState (pack a) b) <$> fromJSVal jv

instance ToJSVal ModulePageState where
  toJSVal (ModulePageState a b) = toJSVal (unpack a, b)

instance FromJSVal PageState where
  fromJSVal jv = do
    vAll <- getProp jv "AllModulesPage"
    vMine <- getProp jv "MyModulesPage"
    vModule <- getProp jv "Mo"
-}
