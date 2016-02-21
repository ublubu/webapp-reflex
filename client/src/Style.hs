module Style where

import Data.Monoid
import qualified Data.Map as M
import Reflex.Dom

type StyleMap = M.Map String String

toStyleString :: StyleMap -> String
toStyleString styleMap = mconcat $ fmap f (M.toList styleMap)
  where f (key, val) = key ++ ":" ++ val ++ ";"

insertStyles :: StyleMap -> M.Map String String -> M.Map String String
insertStyles styles attrs = M.insert "style" (toStyleString styles) attrs

toAttr :: StyleMap -> M.Map String String
toAttr styles = "style" =: (toStyleString styles)

styleIf :: Bool -> StyleMap -> StyleMap
styleIf False _ = mempty
styleIf True x = x

px :: Int -> String
px x = show x ++ "px"

type CssMap = M.Map String StyleMap

toCssString :: CssMap -> String
toCssString cssMap = mconcat $ fmap f (M.toList cssMap)
  where f (key, val) = key ++ " {\n" ++ toStyleString val ++ "\n}\n"
