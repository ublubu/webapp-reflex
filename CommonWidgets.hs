{-# LANGUAGE RecursiveDo #-}

module CommonWidgets where

import Reflex
import Reflex.Dom

import Combinators
import Utils

toggleButton :: (MonadWidget t m) => Bool -> String -> String -> m (Dynamic t Bool)
toggleButton initVal makeTrue makeFalse = mdo
  let tagButton v = fmap . fmap $ const v
      makeTrueButton = tagButton True $ button makeTrue
      makeFalseButton = tagButton False $ button makeFalse
  toggleEvents <- dIf' val makeFalseButton makeTrueButton
  val <- holdDyn initVal toggleEvents
  return val
