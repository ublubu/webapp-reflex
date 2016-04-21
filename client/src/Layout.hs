module Layout where

import qualified Data.Map as M
import Data.Monoid
import Reflex
import Reflex.Dom

import Style
import qualified Styles as S

overlay :: (MonadWidget t m) => m a -> m a
overlay contents =
  elAttr "div" (toAttr $ S.displayFlex
               <> S.fullWindow
               <> S.justifyContent "center"
               <> S.alignItems "center"
               <> S.posFix
               <> S.top 0) contents

centeringDiv :: (MonadWidget t m) => m a -> m a
centeringDiv content =
  divAttr (toAttr $ S.fullWindow
                <> S.displayFlex
                <> S.flexCol
                <> "justify-content" =: "center"
                <> "align-items" =: "center"
               ) content

divAttr :: (MonadWidget t m) => M.Map String String -> m a -> m a
divAttr = elAttr "div"
divAttr' :: (MonadWidget t m) => M.Map String String -> m a -> m (El t, a)
divAttr' = elAttr' "div"

rowDiv :: (MonadWidget t m) => m a -> m a
rowDiv content =
  divAttr (toAttr $ S.displayFlex
               <> S.flexRow) content

columnDiv :: (MonadWidget t m) => m a -> m a
columnDiv content =
  divAttr (toAttr $ S.displayFlex
               <> S.flexCol) content
