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
  listCache <- mapDyn _mcMine cache
  moduleListWidget makeHref paging listCache mtConfig
  where mtConfig = ModuleThumbnailConfig { _mtcDeletable = True
                                         , _mtcEditable = True
                                         , _mtcCloneable = True
                                         }
