{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.App where

import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Database.Persist.Sql (SqlPersistT, ConnectionPool, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager(..), newManager)
import Network.Wai (Middleware)
import Servant
import Servant.Server
import Web.ClientSession

data AppConfig = AppConfig { _appConfigHttpManager :: Manager
                           , _appConfigGoogleClientId :: Text
                           , _appConfigClientSessionKey :: Key
                           , _appConfigFileRoot :: FilePath
                           , _appConfigStaticRoot :: FilePath
                           , _appConfigSqlPool :: ConnectionPool
                           }

-- TODO: just use ServantErr
data AppError = Invalid Text | WrappedServantErr ServantErr
type App = ReaderT AppConfig (ExceptT AppError IO)

instance HasHttpManager AppConfig where
  getHttpManager = _appConfigHttpManager

runApp :: AppConfig -> App a -> EitherT ServantErr IO a
runApp config action = do
  res <- liftIO . runExceptT . flip runReaderT config $ action
  EitherT $ return $ case res of
    Left (Invalid text) -> Left err400 { errBody = encodeUtf8 . fromStrict $ text }
    Left (WrappedServantErr e) -> Left e
    Right a -> Right a

-- TODO: ssl for the Manager
defaultAppConfig :: (MonadIO m) => FilePath -> m AppConfig
defaultAppConfig fileRoot = do
  clientId <- liftIO . fmap T.strip . T.readFile $ fileRoot ++ "/clientid.txt"
  manager <- newManager
  pool <- makePool
  key <- liftIO . getKey $ fileRoot ++ "/sessionkey.txt"
  staticRoot <- liftIO . fmap T.unpack . fmap T.strip . T.readFile $ fileRoot ++ "/staticroot.txt"
  return $ AppConfig manager clientId key fileRoot staticRoot pool

completeFilePath :: AppConfig -> FilePath -> FilePath
completeFilePath config = (++) (_appConfigFileRoot config)

makePool :: (MonadIO m) => m ConnectionPool
makePool = liftIO . runStdoutLoggingT $ createSqlitePool "dev.db" 1

runDb :: (MonadIO m, MonadReader AppConfig m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks _appConfigSqlPool
  liftIO $ runSqlPool query pool

type ExceptSqlT m = ExceptT AppError (SqlPersistT m)

runDbExcept' :: (MonadIO m, MonadReader AppConfig m) => ExceptT e (SqlPersistT IO) b -> m (Either e b)
runDbExcept' = runDb . runExceptT

runDbExcept :: (MonadIO m, MonadReader AppConfig m, MonadError e m) => ExceptT e (SqlPersistT IO) b -> m b
runDbExcept query = do
  result <- runDbExcept' query
  case result of Left err -> throwError err
                 Right x -> return x

rightDb :: (Monad m) => SqlPersistT m b -> ExceptT e (SqlPersistT m) b
rightDb = ExceptT . fmap Right

exceptDb :: (Monad m) => (b -> Either e c) -> SqlPersistT m b -> ExceptT e (SqlPersistT m) c
exceptDb condition = ExceptT . fmap condition

throwWrapped :: (MonadError AppError m) => ServantErr -> m a
throwWrapped = throwError . WrappedServantErr

firstOr404 :: (MonadError AppError m) => [a] -> m a
firstOr404 [] = throwWrapped err404
firstOr404 (x:_) = return x

justOr404 :: (MonadError AppError m) => Maybe a -> m a
justOr404 = maybe (throwWrapped err404) return
