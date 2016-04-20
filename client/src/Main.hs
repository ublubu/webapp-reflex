{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom

import Callback
import Combinators
import CommonWidgets
import History
import Pages
import Style
import qualified Styles as S
import Utils

import API.SignIn

main :: IO ()
main = mainWidget $ pathnameHistoryWidget parsePath (\i -> "/" ++ show i) dummyCounter
  where parsePath "/" = 0
        parsePath ('/':x) = read x
        parsePath _ = error "invalid path"

dummyCounter :: (MonadWidget t m)
             => Dynamic t Int
             -> m (Event t Int)
dummyCounter route = do
  btnPresses <- ecDyn' (button . show) route
  nextRoute <- mapDyn (+1) route
  return $ tag (current nextRoute) btnPresses
