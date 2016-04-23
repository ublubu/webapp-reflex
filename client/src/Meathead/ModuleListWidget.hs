{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Meathead.ModuleListWidget where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Text (Text, pack, unpack)

import API.Module

import Apply
import Combinators
import CommonWidgets

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
                 => Dynamic t (PageState -> Text)
                 -> Dynamic t PagingState -- route-ish
                 -> Dynamic t ModuleListCache -- backend interface
                 -> ModuleThumbnailConfig
                 -- bubbling up route-ish and backend interface events
                 -> m (BubbleApp' t)
moduleListWidget makeHref paging cache mtConfig = do
  modules <- combineDyn maybeModulePage paging cache
  dWhenJust modules f
  where f :: Dynamic t [ModuleView] -> m (BubbleApp t ())
        f mdlsD = ec $ (\mdls -> ecLeftmost <$> mapM (moduleThumbnailWidget makeHref mtConfig) mdls) @/ mdlsD

moduleThumbnailWidget :: (MonadWidget t m)
                      => Dynamic t (PageState -> Text)
                      -> ModuleThumbnailConfig
                      -> ModuleView
                      -> m (BubbleApp' t)
moduleThumbnailWidget makeHref ModuleThumbnailConfig{..} (ModuleMeta{..}, mParentGuid, ModuleEdit{..}) = do
  titleClicks <- el "p" $ moduleLink makeHref _mmGuid _meTitle
  parentClicks <- el "p" $ maybe ecNever (\pg -> moduleLink makeHref pg pg) mParentGuid
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
