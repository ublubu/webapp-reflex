{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Data.Monoid

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

main :: IO ()
main = mainWidgetWithHead headEl $ mdo
  signIns <- signInEvent
  centeringDiv $ rowDiv $ do
    columnDiv gSignInButton
    columnDiv $ pathnameHistoryWidget parsePath (\i -> "/" ++ show i) dummyCounter

dummyCounter :: (MonadWidget t m)
             => Dynamic t Int
             -> m (Event t Int)
dummyCounter route = do
  btnPresses <- ecDyn' (button . show) route
  nextRoute <- mapDyn (+1) route
  return $ tag (current nextRoute) btnPresses

parsePath :: String -> Int
parsePath "/" = 0
parsePath ('/':x) = read x
parsePath _ = error "invalid path"

css :: String
css = toCssString $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  gSignInHeadEl
  el "style" $ text css
