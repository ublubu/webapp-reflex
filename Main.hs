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
main = mainWidget $ historyWidget pageRouter IntroPage

test :: (MonadWidget t m) => m ()
test = do
  value <- mapDyn fromBool =<< toggleButton False "turn on" "turn off"
  dWhenJust value f
  where f x = do
          content <- mapDyn show x
          dynText content
        fromBool False = Nothing
        fromBool True = Just ()

articlePage :: (MonadWidget t m) => Dynamic t ArticleState -> m (Routing t PageState ())
articlePage articleState = do
  article <- fmap nubDyn $ mapDyn _articleContents articleState
  pageNumber <- fmap nubDyn $ mapDyn _articlePageNumber articleState
  makePageNumberRoute <- mapDyn (\art -> (\i -> ArticlePage $ ArticleState art i)) article

  dyn =<< mapDyn (el "div" . textT . _articleTitle) article
  dyn =<< mapDyn (el "div" . textT . _articleBody) article

  introRoutes <- fmap (routing . eventValue IntroPage) $ button "back to intro"
  pageNumberRoutes <- articleFlipper pageNumber makePageNumberRoute
  return . onlyRouting $ rcFactor [introRoutes, pageNumberRoutes]

articleFlipper :: (MonadWidget t m) => Dynamic t Int -> Dynamic t (Int -> r) -> m (Routing t r ())
articleFlipper pageNumber makePageNumberRoute = do
  nextPageNumber <- mapDyn (+1) pageNumber
  nextPageRoute <- combineDyn ($) makePageNumberRoute nextPageNumber

  el "div" $ do
    text "page: "
    void $ ecDyn' (text . show) pageNumber
    fmap (routing . tag (current nextPageRoute)) $ button "next page"

introPage :: (MonadWidget t m) => Dynamic t [ArticleState] -> m (Routing t PageState ())
introPage articleStates = do
  el "div" (text "Intro!")
  ecDyn' (fmap (onlyRouting . rcFactor) . mapM articleButton) articleStates

articleButton :: (MonadWidget t m) => ArticleState -> m (Routing t PageState ())
articleButton a@ArticleState{..} =
  fmap (routing . eventValue (ArticlePage a)) $ buttonT (_articleTitle _articleContents)

pageRouter :: (MonadWidget t m) => Dynamic t PageState -> m (Event t PageState)
pageRouter pageState = do
  articlePageState <- fmap nubDyn $ mapDyn (^? _ArticlePage) pageState
  introPageState <- fmap nubDyn $ mapDyn (^? _IntroPage) pageState
  let articles = constDyn someArticleStates

  articleRoutes <- dWhenJust articlePageState articlePage
  introRoutes <- dWhenJust introPageState (const $ introPage articles)
  return . _rRoutes $ rcFactor [articleRoutes, introRoutes]

articleA = Article "Day One" "I ate a banana."
articleB = Article "Day Two" "I pooped."

articleStateA = ArticleState articleA 1
articleStateB = ArticleState articleB 1

someArticleStates :: [ArticleState]
someArticleStates = [articleStateA, articleStateB]

historyWidget :: (MonadWidget t m) => (Dynamic t r -> m (Event t r)) -> r -> m ()
historyWidget f route0 = mdo
  let doPushStates = fmap (:) (tag (current currentRoute) newStates)
      doPopStates = fmap (const tail) popStates
      updates = leftmost [doPushStates, doPopStates]
  history <- foldDyn ($) [] updates
  lastRoute <- mapDyn head history
  currentRoute <- holdDyn route0 $ leftmost [popStates, newStates]

  popStates <- fmap (tag $ current lastRoute) $ button "back"
  newStates <- f currentRoute
  noContents

routedWidget :: (MonadWidget t m) => (Dynamic t r -> m (Event t r)) -> r -> m ()
routedWidget f x0 = mdo
  xs <- f x
  x <- holdDyn x0 xs
  noContents
