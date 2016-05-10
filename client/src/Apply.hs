{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Apply where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Data.Monoid

import Utils

{-
Here are some Applicative-inspired combinators for widget-creating functions.

The symbol [!@$#*] refers to whether the arguments and result are monadic values

like $
! :: f -> x -> r

like <$>
  (squiggly symbols)
@ :: f -> x -> m r
$ :: f -> m x -> m r

like <*>
  (line-intersection symbols)
# :: m f -> x -> m r
* :: m f -> m x -> m r

Using regex-ish notation below:

The prefix [/]? refers to whether [f] is in a Dynamic.
The suffix [/|]? refers to whether [x] is Dynamic (/), static (|), or matches [f]'s arg type (absent)

The sequence of operators follows these rules:
[!]* [@$] [#*]*
Following [contains /], all operators [prefixed /]

______________________
Extracting the result:

If the operator sequence contains [@$#*], then the result is wrapped in an extra `m`
If the operator sequence contains [/], then the result is wrapped in an extra `Dynamic t`

To extract an EventContainer from a sequence of [@$#*] AND [/], use `ec`
To extract anything from a sequence of [@$#*] WITHOUT [/], use `join`
-}
-- TODO: see if these operators can be typeclassed together
-- TODO: investigate Data.Functor.Compose

------------------------------------------------------------------------------------------
-- pure
------------------------------------------------------------------------------------------

infixl 4 !|
(!|) :: (Reflex t) => (Dynamic t a -> b) -> a -> b
(!|) f = f . constDyn

infixl 4 <!>
(<!>) :: (a -> b) -> a -> b
f <!> x = f x

------------------------------------------------------------------------------------------
-- monadic results
------------------------------------------------------------------------------------------

infixl 4 @/
(@/) :: (MonadWidget t m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
(@/) = mapDyn

infixl 4 /@/
(/@/) :: (MonadWidget t m) => Dynamic t (a -> b) -> Dynamic t a -> m (Dynamic t b)
(/@/) = combineDyn ($)

infixl 4 /@
(/@) :: (MonadWidget t m) => Dynamic t (a -> b) -> a -> m (Dynamic t b)
f /@ x = mapDyn ($ x) f

infixl 4 /@|
(/@|) :: (MonadWidget t m) => Dynamic t (Dynamic t a -> b) -> a -> m (Dynamic t b)
f /@| x = mapDyn ($ constDyn x) f

------------------------------------------------------------------------------------------
-- monadic values on the left
------------------------------------------------------------------------------------------

infixl 4 #/
(#/) :: (MonadWidget t m) => m (a -> b) -> Dynamic t a -> m (Dynamic t b)
fM #/ xD = do
  f <- fM
  f @/ xD

infixl 4 <#>
(<#>) :: (Functor m) => m (a -> b) -> a -> m b
(<#>) = flip (fmap . flip ($))

infixl 4 #|
(#|) :: (Monad m, Reflex t) => m (Dynamic t a -> b) -> a -> m b
fM #| x = do
  f <- fM
  return $ f !| x

infixl 4 /#/
(/#/) :: (MonadWidget t m) => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
fM /#/ xD = do
  f <- fM
  f /@/ xD

infixl 4 /#
(/#) :: (MonadWidget t m) => m (Dynamic t (a -> b)) -> a -> m (Dynamic t b)
fM /# x = do
  f <- fM
  f /@ x

infixl 4 /#|
(/#|) :: (MonadWidget t m) => m (Dynamic t (Dynamic t a -> b)) -> a -> m (Dynamic t b)
fM /#| x = do
  f <- fM
  f /@| x

------------------------------------------------------------------------------------------
-- monadic values on the right
------------------------------------------------------------------------------------------

infixl 4 $/
($/) :: (MonadWidget t m) => (a -> b) -> m (Dynamic t a) -> m (Dynamic t b)
f $/ xD = mapDyn f =<< xD

infixl 4 $|
($|) :: (Monad m, Reflex t) => (Dynamic t a -> b) -> m a -> m b
f $| xM = do
  x <- xM
  return $ f !| x

infixl 4 /$/
(/$/) :: (MonadWidget t m) => Dynamic t (a -> b) -> m (Dynamic t a) -> m (Dynamic t b)
f /$/ xDM = do
  xD <- xDM
  f /@/ xD

infixl 4 /$
(/$) :: (MonadWidget t m) => Dynamic t (a -> b) -> m a -> m (Dynamic t b)
f /$ xM = do
  x <- xM
  f /@ x

infixl 4 /$|
(/$|) :: (MonadWidget t m) => Dynamic t (Dynamic t a -> b) -> m a -> m (Dynamic t b)
f /$| xM = do
  x <- xM
  f /@| x

------------------------------------------------------------------------------------------
-- monadic values on both sides
------------------------------------------------------------------------------------------

infixl 4 */
(*/) :: (MonadWidget t m) => m (a -> b) -> m (Dynamic t a) -> m (Dynamic t b)
fM */ xDM = do
  f <- fM
  xD <- xDM
  f @/ xD

infixl 4 *|
(*|) :: (Monad m, Reflex t) => m (Dynamic t a -> b) -> m a -> m b
fM *| xM = do
  f <- fM
  x <- xM
  return $ f !| x

infixl 4 /*/
(/*/) :: (MonadWidget t m) => m (Dynamic t (a -> b)) -> m (Dynamic t a) -> m (Dynamic t b)
fM /*/ xDM = do
  f <- fM
  xD <- xDM
  f /@/ xD

infixl 4 /*
(/*) :: (MonadWidget t m) => m (Dynamic t (a -> b)) -> m a -> m (Dynamic t b)
fM /* xM = do
  f <- fM
  x <- xM
  f /@ x

infixl 4 /*|
(/*|) :: (MonadWidget t m) => m (Dynamic t (Dynamic t a -> b)) -> m a -> m (Dynamic t b)
fM /*| xM = do
  f <- fM
  x <- xM
  f /@| x
