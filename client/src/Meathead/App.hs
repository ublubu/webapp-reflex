{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Meathead.App where

import Reflex hiding (count)
import Reflex.Dom hiding (count)

import Control.Lens
import Data.Text (Text)

import Combinators
import CommonWidgets

import Meathead.Pages
import Meathead.ModuleCache

type BubbleApp t e =
  Bubbling t (Event t PageState, Event t ModuleRequest) e

type BubbleApp' t = BubbleApp t ()

appLink :: (MonadWidget t m)
           => Dynamic t (PageState -> Text)
           -> PageState
           -> Text
           -> m (BubbleApp' t)
appLink makeHref link label = do
  anchorD <- mapDyn (\mkHref -> Anchor label (mkHref link) link) makeHref
  bubbleWith _1 =<< dynAnchorT anchorD
