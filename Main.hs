{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Text (unpack)

import Combinators
import CommonWidgets
import Pages
import Utils

main :: IO ()
main = mainWidget $ routedWidget pageRouter IntroPage

articlePage :: (MonadWidget t m) => Article -> m (Routing t PageState ())
articlePage Article{..} = do
  el "div" (textT $ _articleTitle)
  el "div" (textT $ _articleBody)
  fmap (routing . eventValue IntroPage) $ button "back to intro"

introPage :: (MonadWidget t m) => [ArticleState] -> m (Routing t PageState ())
introPage articleStates = do
  el "div" (text "Intro!")
  fmap (onlyRouting . rcFactor) $ mapM articleButton articleStates

articleButton :: (MonadWidget t m) => ArticleState -> m (Routing t PageState ())
articleButton a@ArticleState{..} =
  let buttonLabel = (show _articleId ++ ": " ++ unpack (a ^. articleContents . articleTitle))
  in fmap (routing . eventValue (ArticlePage a)) $ button buttonLabel

pageRouter :: (MonadWidget t m) => PageState -> m (Event t PageState)
pageRouter IntroPage = fmap _rRoutes $ introPage someArticleStates
pageRouter (ArticlePage a) = fmap _rRoutes $ articlePage (a ^. articleContents)

articleA = Article "Day One" "I ate a banana."
articleB = Article "Day Two" "I pooped."

articleStateA = ArticleState 1 articleA
articleStateB = ArticleState 2 articleB

someArticleStates :: [ArticleState]
someArticleStates = [articleStateA, articleStateB]

routedWidget :: (MonadWidget t m) => (r -> m (Event t r)) -> r -> m ()
routedWidget f x0 = mdo
  xs <- ecDyn' f x
  x <- holdDyn x0 xs
  noContents
