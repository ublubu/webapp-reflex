{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Code where

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Int
import qualified Data.Function as F
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

import qualified Database.Code as DC
import Database.Esqueleto
import Servant
import Servant.Server

import API.SignIn
import API.Code
import Server.App
import Server.SignIn

codeFromDb :: Entity DC.Code -> CodeView
codeFromDb = codeFromDb' . entityVal

codeFromDb' :: DC.Code -> CodeView
codeFromDb' code@DC.Code{..} = (meta, codeFromDb'' code)
  where meta = CodeMeta { _cmAuthor = codeAuthorId
                        , _cmGuid = codeGuid
                        }

codeFromDb'' :: DC.Code -> CodeEdit
codeFromDb'' DC.Code{..} =
  CodeEdit { _ceTitle = codeTitle
           , _ceDescription = codeDescription
           , _ceCode = codeCode }

codeToDb :: CodeView -> DC.Code
codeToDb (CodeMeta{..}, CodeEdit{..}) =
  DC.Code { codeGuid = _cmGuid
          , codeAuthorId = _cmAuthor
          , codeTitle = _ceTitle
          , codeDescription = _ceDescription
          , codeCode = _ceCode
          }

-- TODO: add default pagesize to App config?
-- TODO: like withCookieText for offset/count
getCodes :: Maybe Int -> Maybe Int -> App [CodeView]
getCodes _offset count = do
  let offset' = fromMaybe 0 _offset
      count' = fromMaybe 100 count
  codes <- runDb $ select $ from $
    (\c -> do
        limit $ fromIntegral count'
        offset $ fromIntegral offset'
        return c)
  let codes' = fmap codeFromDb codes
  return codes'

myCodes :: Maybe Int -> Maybe Int -> CookieData -> App [CodeView]
myCodes _offset count CookieData{..} = do
  let offset' = fromMaybe 0 _offset
      count' = fromMaybe 100 count
  codes <- runDb $ select $ from $ \c -> do
    where_ (c ^. DC.CodeAuthorId ==. val _cookieDataUserId)
    limit $ fromIntegral count'
    offset $ fromIntegral offset'
    return c
  let codes' = fmap codeFromDb codes
  return codes'

getCode :: Text -> App CodeView
getCode guid = do
  codes <- runDb $ select $ from $ \c -> do
    where_ (c ^. DC.CodeGuid ==. val guid)
    limit 1
    return c
  case codes of [] -> throwWrapped err404
                (c:_) -> return . codeFromDb $ c

postCode :: CodeEdit -> CookieData -> App CodeView
postCode code CookieData{..} = do
  guid <- liftIO $ U.toText <$> U.nextRandom
  let meta = CodeMeta { _cmAuthor = _cookieDataUserId
                      , _cmGuid = guid }
  key <- runDb . insert . codeToDb $ (meta, code)
  return (meta, code)

-- TODO: dedupe put and delete
putCode :: Text -> CodeEdit -> CookieData -> App CodeView
putCode guid code@CodeEdit{..} CookieData{..} = do
  nUpdated <- runDb $ updateCount $ \c -> do
    set c [ DC.CodeTitle =. val _ceTitle
          , DC.CodeDescription =. val _ceDescription
          , DC.CodeCode =. val _ceCode ]
    where_ (c ^. DC.CodeGuid ==. val guid
            &&. c ^. DC.CodeAuthorId ==. val _cookieDataUserId)
  if nUpdated < 1
    then throwWrapped err403 -- can only change your own code
    else return $ (meta, code)
  where meta = CodeMeta { _cmAuthor = _cookieDataUserId
                        , _cmGuid = guid }

deleteCode :: Text -> CookieData -> App ()
deleteCode guid CookieData{..} = do
  nDeleted <- runDb $ deleteCount $ from $ \c -> do
    where_ (c ^. DC.CodeGuid ==. val guid
            &&. c ^. DC.CodeAuthorId ==. val _cookieDataUserId)
  if nDeleted < 1
    then throwWrapped err403 -- can only delete your own code
    else return ()

codeServer :: ServerT CodeAPI App
codeServer =
  getCodes
  :<|> (withCookieText1 postCode :<|> getCode :<|> withCookieText2 putCode :<|> withCookieText1 deleteCode)
  :<|> withCookieText2 myCodes
