{-# LANGUAGE ScopedTypeVariables #-}

module Meathead.MyModulesWidget where

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

myModulesWidget :: forall t m. (MonadWidget t m)
                => Dynamic t (PageState -> Text)
                -> Dynamic t PagingState
                -> Dynamic t ModuleCache
                -> m (BubbleApp' t)
myModulesWidget makeHref paging cache = do
  ec $ withCachedModuleList _mcMine ReadMyModules @/ paging /# cache /# makeWidget
  where makeWidget = moduleListWidget makeHref mtConfig
        mtConfig = ModuleThumbnailConfig { _mtcDeletable = True
                                         , _mtcEditable = True
                                         , _mtcCloneable = True
                                         }
