{-# LANGUAGE ScopedTypeVariables #-}

module Meathead.AllModulesWidget where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Text (Text)

import Apply
import Combinators
import CommonWidgets

import Meathead.App
import Meathead.ModuleCache
import Meathead.ModuleListWidget
import Meathead.Pages

allModulesWidget :: forall t m. (MonadWidget t m)
                 => Dynamic t (PageState -> Text)
                 -> Dynamic t PagingState
                 -> Dynamic t ModuleCache
                 -> m (BubbleApp' t)
allModulesWidget makeHref paging cache = do
  ec $ withCachedModuleList _mcAll ReadModules @/ paging /# cache /# makeWidget
  where makeWidget = moduleListWidget makeHref mtConfig
        mtConfig = ModuleThumbnailConfig { _mtcDeletable = False
                                         , _mtcEditable = False
                                         , _mtcCloneable = True
                                         }
