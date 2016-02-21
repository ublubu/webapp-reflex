{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Monoid
import Data.Text (unpack)

import Combinators
import CommonWidgets
import Pages
import Style
import qualified Styles as S
import Utils

import API.SignIn

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
    let pageNumberSize = (100, 100)
    text "page: "
    routings <- fmap (routing . tag (current nextPageRoute)) $ button "next page"
    void $ fixedContainer pageNumberSize $ do
      transitionController swipeUp (articleDummyPage pageNumberSize) pageNumber

    return routings

articleDummyPage :: (MonadWidget t m) => (Int, Int) -> Int -> m ()
articleDummyPage (w, h) pageNumber =
  elAttr "div" (toAttr $ S.width w
                <> S.height h
                <> "font-size" =: "10em"
                <> S.backgroundColor "grey")
  $ text (show pageNumber)

fixedContainer :: (MonadWidget t m) => (Int, Int) -> m a -> m a
fixedContainer (w, h) =
  elAttr "div" (toAttr $ S.posRel
                <> S.width w
                <> S.height h)

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

-- TODO: use listHoldWithKey when it comes out
transitionController :: forall a t m b . (EventContainer t m b) => Transitioner a t m b -> (a -> m b) -> Dynamic t a -> m b
transitionController transitioner makeWidget val = mdo
  val0 <- sample (current val)
  let indexedVal0 = (0, val0) :: (Int, a)
  indexedVals <- foldDyn (\v1 (i0, v0) -> (i0 + 1, v1)) indexedVal0 (updated val)

  let inserters = fmap (uncurry M.insert) (updated indexedVals)
      transitioningVals0 = uncurry M.singleton indexedVal0
      updates = leftmost [inserters, removers]
  transitioningVals <- foldDyn ($) transitioningVals0 updates

  itemResultsMapDyn <- list transitioningVals (withDynSample $ transitioner makeWidget)

  removerAndResultDyn <- mapDyn getReplacedItemRemover itemResultsMapDyn
                         :: m (Dynamic t (Event t ((Map Int a -> Map Int a), b)))
  let removerAndResults = switchPromptlyDyn removerAndResultDyn
      removers = fmap fst removerAndResults
  newResults <- ecJoin $ fmap snd removerAndResults
  -- block the results during transitions
  nullResult <- ecNever
  nullResults <- ecJoin $ fmap (const nullResult) inserters
  return $ ecLeftmost [newResults, nullResults]

getReplacedItemRemover :: (Reflex t, Ord k) => Map k (Event t x, b) -> Event t ((Map k a -> Map k a), b)
getReplacedItemRemover triggersAndResults =
  fmap getRemoverAndResult taggedTrigger
  where triggers = M.map fst triggersAndResults
        results = M.map snd triggersAndResults
        taggedTrigger = leftmost . fmap snd $ M.toDescList (tagTriggers triggers)
        getRemoverAndResult key = (deleteLessThan key, results ! key)

tagTriggers :: (Reflex t, Ord k) => Map k (Event t x) -> Map k (Event t k)
tagTriggers = M.mapWithKey (\k evt -> fmap (const k) evt)

deleteLessThan :: (Ord k) => k -> Map k a -> Map k a
deleteLessThan testKey = M.filterWithKey (\k _ -> k >= testKey)

withDynSample :: (MonadWidget t m) => (a -> m b) -> Dynamic t a -> m b
withDynSample makeWidget val =
  makeWidget =<< sample (current val)

type Transitioner a t m b = (a -> m b) -> a -> m (Event t (), b)

noTransition :: (MonadWidget t m) => Transitioner a t m b
noTransition makeWidget val = do
  pb <- getPostBuild
  evt <- makeWidget val
  return (pb, evt)

swipeUp :: (MonadWidget t m) => Transitioner a t m b
swipeUp makeWidget val = mdo
  let loaded = domEvent Load containerEl
      containerStyle = "transition" =: "-webkit-transform 1s"
                       <> "-webkit-transform" =: "translateY(100px)"
                       <> S.posAbs
      containerStyle' = "-webkit-transform" =: "none" <> containerStyle
  styleDyn <- holdDyn containerStyle $ fmap (const containerStyle') loaded
  attrDyn <- mapDyn toAttr styleDyn
  (containerEl, evt) <- elDynAttr' "div" attrDyn $ makeWidget val
  return (loaded, evt)
