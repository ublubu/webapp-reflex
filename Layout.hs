module Layout where

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
