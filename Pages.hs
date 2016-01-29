{-# LANGUAGE TemplateHaskell #-}
module Pages where

import Control.Lens.TH
import Data.Int
import Data.Text (Text)

data Article = Article {
  _articleTitle :: Text,
  _articleBody :: Text
  } deriving (Show, Eq)
makeLenses ''Article

data ArticleState = ArticleState {
  _articleId :: Int64,
  _articleContents :: Article
  } deriving (Show, Eq)
makeLenses ''ArticleState

data PageState = IntroPage | ArticlePage ArticleState
  deriving (Show, Eq)
makePrisms ''PageState

