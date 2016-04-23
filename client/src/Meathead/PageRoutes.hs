{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Meathead.PageRoutes where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text, pack, append, unpack)
import Text.Parsec (runParser, option, count, many1, optionMaybe)
import Text.Parsec.Char
import Text.Parsec.Text

import Meathead.Pages

-- TODO: parse/unparse together?

pagingHrefSegment :: PagingState -> Text
pagingHrefSegment PagingState{..} =
  pack $ "(" ++ show _pagingOffset ++ "+" ++ show _pagingCount ++ ")"

parseDigits :: Parser Int
parseDigits = read <$> many1 digit

parsePagingState :: Parser PagingState
parsePagingState = do
  void $ char '('
  offset <- parseDigits
  void $ char '+'
  count <- parseDigits
  void $ char ')'
  return $ PagingState { _pagingOffset = offset
                       , _pagingCount = count
                       }

parseGuid :: Parser Text
parseGuid =
  fmap pack $ digs 8 +++ hyphen +++ digs 4 +++ hyphen +++ digs 4 +++ hyphen +++ digs 12
  where a +++ b = (++) <$> a <*> b
        digs n = count n alphaNum
        hyphen = (\x -> [x]) <$> char '-'

-- TODO: configure default PagingState somewhere
parseAllModules :: Parser PageState
parseAllModules = do
  void $ string "/all"
  AllModulesPage <$> option (PagingState 0 100) parsePagingState

parseMyModules :: Parser PageState
parseMyModules = do
  void $ string "/mine"
  MyModulesPage <$> option (PagingState 0 100) parsePagingState

parseModulePage :: Parser PageState
parseModulePage = do
  void $ char '/'
  guid <- parseGuid
  editing <- isJust <$> optionMaybe (string "/edit")
  return . ModulePage $ ModulePageState guid editing

parseCreatePage :: Parser PageState
parseCreatePage = do
  void $ string ""
  return $ ModuleCreatePage Nothing

parseClonePage :: Parser PageState
parseClonePage = do
  string "/clone/"
  ModuleCreatePage . Just <$> parseGuid

parseHref :: Parser PageState
parseHref = do
  parseAllModules <|> parseMyModules <|> parseModulePage <|> parseCreatePage <|> parseClonePage

makeHref :: PageState -> Text
makeHref (AllModulesPage paging) =
  "/all" `append` pagingHrefSegment paging
makeHref (MyModulesPage paging) =
  "/mine" `append` pagingHrefSegment paging
makeHref (ModulePage ModulePageState{..}) =
  let prefix = "/" `append` _mpGuid
  in if _mpEditing then prefix `append` "/edit"
     else prefix
makeHref (ModuleCreatePage Nothing) =
  "/create"
makeHref (ModuleCreatePage (Just guid)) =
  "/clone/" `append` guid

fromPath :: String -> PageState
fromPath = f . runParser parseHref () "" . pack
  where f (Left x) = error $ show x
        f (Right x) = x

toPath :: PageState -> String
toPath = unpack . makeHref
