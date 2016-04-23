{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

module CommonWidgets where

import Reflex
import Reflex.Dom

import Control.Monad
import Data.Monoid
import Data.Text (Text, unpack, pack)

import Apply
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

textT :: (MonadWidget t m) => Text -> m ()
textT = text . unpack

dynTextT :: (MonadWidget t m) => Dynamic t Text -> m ()
dynTextT = dynText <=< mapDyn unpack

buttonT :: (MonadWidget t m) => Text -> m (Event t ())
buttonT = button . unpack

onceButton :: (MonadWidget t m) => String -> m (Event t ())
onceButton label = mdo
  clicks <- buttonWhile unclicked label
  unclicked <- falseAfter clicks
  return clicks

dynButtonT :: (MonadWidget t m) => Dynamic t Text -> m (Event t ())
dynButtonT = dynWidgetEvents <=< mapDyn buttonT

simpleTextInput :: (MonadWidget t m) => Text -> m (Dynamic t Text)
simpleTextInput initVal = mapDyn pack =<< _textInput_value <$> textInput conf
  where conf = def { _textInputConfig_initialValue = unpack initVal }

simpleTextInput' :: (MonadWidget t m) => m (Dynamic t Text)
simpleTextInput' = simpleTextInput ""

data Anchor a =
  Anchor { _anchorText :: Text
         , _anchorHref :: Text
         , _anchorPayload :: a
         } deriving (Show, Eq)

anchorT :: (MonadWidget t m)
        => Anchor a
        -> m (Event t a)
anchorT Anchor{..} = do
  (elem, _) <- elAttr' "a" ("href" =: unpack _anchorHref) $ textT _anchorText
  return $ const _anchorPayload <$> domEvent Click elem

dynAnchorT :: (MonadWidget t m)
           => Dynamic t (Anchor a)
           -> m (Event t a)
dynAnchorT =
  dynWidgetEvents <=< mapDyn anchorT

paraT :: (MonadWidget t m) => Text -> m ()
paraT = el "p" . textT

paraS :: (MonadWidget t m) => String -> m ()
paraS = el "p" . text

dynParaT :: (MonadWidget t m) => Dynamic t Text -> m ()
dynParaT = el "p" . dynTextT

falseAfter :: (MonadWidget t m)
           => Event t x
           -> m (Dynamic t Bool)
falseAfter freeze =
  holdDyn True =<< headE (eventValue False freeze)

buttonWhile :: (MonadWidget t m)
            => Dynamic t Bool
            -> String
            -> m (Event t ())
buttonWhile enableD label = do
  let f enable = "type" =: "button" <> "disabled" =: ""
  (elem, _) <- join $ elDynAttr' "button" <$> mapDyn f enableD <#> text label
  return $ domEvent Click elem
