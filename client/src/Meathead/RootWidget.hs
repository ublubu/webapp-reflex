{-# LANGUAGE OverloadedStrings #-}

module Meathead.RootWidget where

import Reflex
import Reflex.Dom

import Control.Lens
import Data.Text (Text)

import Combinators

import Meathead.App
import Meathead.Pages
import Meathead.ModuleCache
import Meathead.AllModulesWidget
import Meathead.MyModulesWidget
import Meathead.ModuleWidget

rootWidget :: (MonadWidget t m)
           => Maybe Text -- userId
           -> Dynamic t (PageState -> Text)
           -> Dynamic t PageState
           -> Dynamic t ModuleCache
           -> m (BubbleApp' t)
rootWidget mUserId makeHref routeD cache = do
  navBubbles <- el "div" $ do
    allClicks <- appLink makeHref (pageAllModules 0 100) "all"
    text " "
    myClicks <- appLink makeHref (pageMyModules 0 100) "mine"
    return $ ecConcat [allClicks, myClicks]
  pageBubbles <- dCase routeD
    [ _AllModulesPage `DCase` \page -> allModulesWidget makeHref page cache
    , _MyModulesPage `DCase` \page -> myModulesWidget makeHref page cache
    , _ModulePage `DCase` moduleWidget mUserId makeHref cache
    , _ModuleCreatePage `DCase` moduleCreationWidget cache
    ]
  return $ ecConcat [pageBubbles, navBubbles]
