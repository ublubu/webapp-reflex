{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Module where

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Int
import qualified Data.Function as F
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

import qualified Database.Module as DC
import Database.Esqueleto
import Servant
import Servant.Server

import API.SignIn
import API.Module
import Server.App
import Server.SignIn

moduleFromDb :: Entity DC.Module -> ModuleView
moduleFromDb = moduleFromDb' . entityVal

moduleFromDb' :: DC.Module -> ModuleView
moduleFromDb' module_@DC.Module{..} =
  (meta, moduleParentGuid, moduleFromDb'' module_)
  where meta = ModuleMeta { _mmAuthor = moduleAuthorId
                          , _mmGuid = moduleGuid
                          }

moduleFromDb'' :: DC.Module -> ModuleEdit
moduleFromDb'' DC.Module{..} =
  ModuleEdit { _meTitle = moduleTitle
             , _meDescription = moduleDescription
             , _meCode = moduleCode }

moduleToDb :: ModuleView -> DC.Module
moduleToDb (ModuleMeta{..}, parentGuid, ModuleEdit{..}) =
  DC.Module { moduleGuid = _mmGuid
            , moduleParentGuid = parentGuid
            , moduleAuthorId = _mmAuthor
            , moduleTitle = _meTitle
            , moduleDescription = _meDescription
            , moduleCode = _meCode
            }

-- TODO: add default pagesize to App config?
-- TODO: bounded offset/pageSize
-- TODO: like withCookieText for offset/count
getModules :: Maybe Int -> Maybe Int -> App [ModuleView]
getModules _offset count = do
  let offset' = fromMaybe 0 _offset
      count' = fromMaybe 100 count
  modules <- runDb $ select $ from $
    (\c -> do
        limit $ fromIntegral count'
        offset $ fromIntegral offset'
        return c)
  let modules' = fmap moduleFromDb modules
  return modules'

myModules :: Maybe Int -> Maybe Int -> CookieData -> App [ModuleView]
myModules _offset count CookieData{..} = do
  let offset' = fromMaybe 0 _offset
      count' = fromMaybe 100 count
  modules <- runDb $ select $ from $ \c -> do
    where_ (c ^. DC.ModuleAuthorId ==. val _cookieDataUserId)
    limit $ fromIntegral count'
    offset $ fromIntegral offset'
    return c
  let modules' = fmap moduleFromDb modules
  return modules'

getModule :: Text -> App ModuleView
getModule guid = do
  modules <- runDb $ select $ from $ \c -> do
    where_ (c ^. DC.ModuleGuid ==. val guid)
    limit 1
    return c
  case modules of [] -> throwWrapped err404
                  (c:_) -> return . moduleFromDb $ c

postModule :: ModuleCreate -> CookieData -> App ModuleView
postModule (parentGuid, module_) CookieData{..} = do
  guid <- liftIO $ U.toText <$> U.nextRandom
  let meta = ModuleMeta { _mmAuthor = _cookieDataUserId
                        , _mmGuid = guid }
  key <- runDb . insert . moduleToDb $ (meta, parentGuid, module_)
  return (meta, parentGuid, module_)

-- TODO: dedupe put and delete
putModule :: Text -> ModuleEdit -> CookieData -> App ModuleEditView
putModule guid module_@ModuleEdit{..} CookieData{..} = do
  nUpdated <- runDb $ updateCount $ \c -> do
    set c [ DC.ModuleTitle =. val _meTitle
          , DC.ModuleDescription =. val _meDescription
          , DC.ModuleCode =. val _meCode ]
    where_ (c ^. DC.ModuleGuid ==. val guid
            &&. c ^. DC.ModuleAuthorId ==. val _cookieDataUserId)
  if nUpdated < 1
    then throwWrapped err403 -- can only change your own module
    else return $ (meta, module_)
  where meta = ModuleMeta { _mmAuthor = _cookieDataUserId
                        , _mmGuid = guid }

deleteModule :: Text -> CookieData -> App ()
deleteModule guid CookieData{..} = do
  nDeleted <- runDb $ deleteCount $ from $ \c -> do
    where_ (c ^. DC.ModuleGuid ==. val guid
            &&. c ^. DC.ModuleAuthorId ==. val _cookieDataUserId)
  if nDeleted < 1
    then throwWrapped err403 -- can only delete your own module
    else return ()

moduleServer :: ServerT ModuleAPI App
moduleServer =
  getModules
  :<|> (withCookieText1 postModule :<|> getModule :<|> withCookieText2 putModule :<|> withCookieText1 deleteModule)
  :<|> withCookieText2 myModules
