{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Meathead.ModuleListWidget where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)

import API.Module

import Apply
import Combinators
import CommonWidgets
import Utils

import Meathead.App
import Meathead.ModuleCache
import Meathead.ModuleWidget
import Meathead.Pages

data ModuleThumbnailConfig =
  ModuleThumbnailConfig { _mtcDeletable :: Bool
                        , _mtcEditable :: Bool
                        , _mtcCloneable :: Bool
                        } deriving (Show, Eq)
makeLenses ''ModuleThumbnailConfig

moduleListWidget :: forall t m. (MonadWidget t m)
                 => Dynamic t (PageState -> Text) -- making hrefs
                 -> ModuleThumbnailConfig
                 -> Dynamic t [ModuleView]
                 -> m (BubbleApp' t)
moduleListWidget makeHref mtConfig modulesD = do
  ec $ (\mdls -> ecLeftmost <$> mapM (moduleThumbnailWidget makeHref mtConfig) mdls) @/ modulesD

moduleThumbnailWidget :: (MonadWidget t m)
                      => Dynamic t (PageState -> Text)
                      -> ModuleThumbnailConfig
                      -> ModuleView
                      -> m (BubbleApp' t)
moduleThumbnailWidget makeHref ModuleThumbnailConfig{..} (ModuleMeta{..}, mParentGuid, ModuleEdit{..}) = do
  titleClicks <- el "p" $ do
    text "title: "
    moduleLink makeHref _mmGuid _meTitle
  let f pg = el "p" $ do
        text "parent: "
        moduleLink makeHref pg pg
  parentClicks <- maybe ecNever f mParentGuid
  delClicks <- whenE _mtcDeletable $ deleteModuleLink _mmGuid
  editClicks <- whenE _mtcEditable $ editModuleLink _mmGuid
  return $ ecLeftmost [titleClicks, parentClicks, delClicks]

deleteModuleLink :: (MonadWidget t m)
                 => Text
                 -> m (BubbleApp' t)
deleteModuleLink guid =
  bubbleWith _2 =<< fmap (const $ DeleteModule guid) <$> button "delete"

editModuleLink :: (MonadWidget t m)
               => Text
               -> m (BubbleApp' t)
editModuleLink guid =
  bubbleWith _1 =<< fmap (const $ pageEditModule guid) <$> button "edit"

withCachedModuleList :: forall t m a. (EventContainer t m a)
                     => (ModuleCache -> ModuleListCache)
                     -> (PagingState -> ModuleRequest)
                     -> PagingState
                     -> Dynamic t ModuleCache
                     -> (Dynamic t [ModuleView] -> m (BubbleApp t a))
                     -> m (BubbleApp t a)
withCachedModuleList = withCachedModuleList_ return

withCachedModuleList_ :: forall t m a x. (EventContainer t m a)
                      => (Dynamic t (Maybe [ModuleView]) -> m (Dynamic t (Maybe [ModuleView])))
                      -> (ModuleCache -> ModuleListCache)
                      -> (PagingState -> ModuleRequest)
                      -> PagingState
                      -> Dynamic t ModuleCache
                      -> (Dynamic t [ModuleView] -> m (BubbleApp t a))
                      -> m (BubbleApp t a)
withCachedModuleList_ filterListD _listCache makeRequest paging cache makeWidget = do
  -- fetch the module list
  listD <- filterListD =<< maybeModulePage paging $/ mapDyn _listCache cache
  list_ <- sample . current $ listD
  readList <- maybe (fire $ makeRequest paging) (const ecNever) list_
  childEvents <- dWhenJust listD makeWidget
  return $ childEvents & bBubble . _2 %~ ecCombine readList
