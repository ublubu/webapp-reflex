module Styles where

import Data.Monoid
import qualified Data.Map as M
import Reflex.Dom ((=:))
import Style

width :: Int -> StyleMap
width w = width' $ px w

width' :: String -> StyleMap
width' w = "width" =: w

height :: Int -> StyleMap
height h = height' $ px h

height' :: String -> StyleMap
height' h = "height" =: h

minWidth :: Int -> StyleMap
minWidth w = minWidth' $ px w

minWidth' :: String -> StyleMap
minWidth' w = "min-width" =: w

minHeight :: Int -> StyleMap
minHeight h = minHeight' $ px h

minHeight' :: String -> StyleMap
minHeight' h = "min-height" =: h

posRel :: StyleMap
posRel = pos "relative"

borderBox :: StyleMap
borderBox = "box-sizing" =: "border-box"

posAbs :: StyleMap
posAbs = pos "absolute"

pos :: String -> StyleMap
pos p = "position" =: p

posFix :: StyleMap
posFix = pos "fixed"

left :: Int -> StyleMap
left x = left' $ px x

left' :: String -> StyleMap
left' x = "left" =: x

top :: Int -> StyleMap
top y = top' $ px y

top' :: String -> StyleMap
top' y = "top" =: y

backgroundColor :: String -> StyleMap
backgroundColor color = "background-color" =: color

border :: String -> StyleMap
border b = "border" =: b

preserve3d :: StyleMap
preserve3d = "transform-style" =: "preserve-3d"

transform :: String -> StyleMap
transform t = "transform" =: t

display :: String -> StyleMap
display d = "display" =: d

displayFlex :: StyleMap
displayFlex = display "flex"

displayInlineFlex :: StyleMap
displayInlineFlex = display "inline-flex"

displayInlineBlock :: StyleMap
displayInlineBlock = display "inline-block"

alignItems :: String -> StyleMap
alignItems a = "align-items" =: a

alignItemsCenter :: StyleMap
alignItemsCenter = alignItems "center"

justifyContent :: String -> StyleMap
justifyContent j = "justify-content" =: j

justifyContentCenter :: StyleMap
justifyContentCenter = justifyContent "center"

noMargin :: StyleMap
noMargin = "margin" =: "0"

noPadding :: StyleMap
noPadding = "padding" =: "0"

fullWidth :: StyleMap
fullWidth = width' "100%"

fullHeight :: StyleMap
fullHeight = height' "100%"

fullWindow :: StyleMap
fullWindow = fullHeight <> fullWidth
