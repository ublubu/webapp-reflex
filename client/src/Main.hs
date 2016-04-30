{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Data.Monoid

import Apply
import Callback
import Combinators
import CommonWidgets
import History
import Layout
import SignIn
import Style
import qualified Styles as S
import Utils

import GoogleSignIn

import API.SignIn

import Meathead.App
import Meathead.RootWidget
import Meathead.ModuleCache
import Meathead.PageRoutes

main :: IO ()
main = mainWidgetWithHead headEl $ mdo
  pageStateD <- unitHistoryWidget fromPath toPath pushStates
  moduleCacheD <- moduleCache emptyModuleCache moduleRequests
  performEvent_ $ liftIO . print <$> updated pageStateD
  performEvent_ $ liftIO . print <$> moduleRequests

  signIns <- signInEvent
  mUserIdD <- holdDyn Nothing (Just . _cookieDataUserId <$> signIns)

  -- visible widgets
  columnDiv gSignInButton
  Bubbling (pushStates, moduleRequests) () <-
    ec $ rootWidget @/ mUserIdD /#| makeHref /# pageStateD /# moduleCacheD

  return ()

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  gSignInHeadEl
  el "style" $ text css
