{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Meathead.Pages where

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Prim

import Control.Lens.TH
import Control.Monad
import Data.Text (Text, pack, unpack)
import Data.Aeson

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
  | ErrorPage
  deriving (Show, Eq)
makePrisms ''PageState

pageAllModules :: Int -> Int -> PageState
pageAllModules offset count =
  AllModulesPage $ PagingState offset count

pageMyModules :: Int -> Int -> PageState
pageMyModules offset count =
  MyModulesPage $ PagingState offset count

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

instance FromJSVal PagingState where
  fromJSVal jv = fmap (uncurry PagingState) <$> fromJSVal jv

instance ToJSON PagingState where
  toJSON (PagingState a b) = toJSON (a, b)

instance ToJSVal PagingState where
  toJSVal (PagingState a b) = toJSVal (a, b)

instance FromJSVal ModulePageState where
  fromJSVal jv = fmap (\(a, b) -> ModulePageState (pack a) b) <$> fromJSVal jv

instance ToJSON ModulePageState where
  toJSON (ModulePageState a b) = toJSON (a, b)

instance ToJSVal ModulePageState where
  toJSVal (ModulePageState a b) = toJSVal (unpack a, b)

-- TODO: differentiate between fromJSVal error and intentional ErrorPage
instance FromJSVal PageState where
  fromJSVal jv = do
    vAll <- getProp jv "AllModulesPage"
    vMine <- getProp jv "MyModulesPage"
    vModule <- getProp jv "ModulePage"
    vCreate <- getProp jv "ModuleCreatePage"
    vError <- getProp jv "ErrorPage"
    if | exists vAll -> fmap AllModulesPage <$> fromJSVal vAll
       | exists vMine -> fmap MyModulesPage <$> fromJSVal vMine
       | exists vModule -> fmap ModulePage <$> fromJSVal vModule
       | exists vCreate -> fmap ModuleCreatePage <$> fromJSVal vCreate
       | exists vError -> return $ return ErrorPage
       | otherwise -> return $ return ErrorPage
    where exists = not . isUndefined

instance ToJSON PageState where
  toJSON (AllModulesPage page) =
    object ["AllModulesPage" .= page]
  toJSON (MyModulesPage page) =
    object ["MyModulesPage" .= page]
  toJSON (ModulePage mp) =
    object ["ModulePage" .= mp]
  toJSON (ModuleCreatePage parent) =
    object ["ModuleCreatePage" .= parent]
  toJSON (ErrorPage) =
    object ["ErrorPage" .= True]

instance ToJSVal PageState where
  toJSVal pageState = toJSVal_aeson pageState

roundTripJSVal :: (FromJSVal a, ToJSVal a) => a -> IO (Maybe a)
roundTripJSVal = fromJSVal <=< toJSVal
