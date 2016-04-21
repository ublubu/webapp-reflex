{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Database.Persist.Sql (runSqlPool)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Servant
import Servant.Server

import API.Module
import API.SignIn
import Database.Module
import Server.App
import Server.Module
import Server.Raw
import Server.SignIn

type StaticAPI = Raw
type API = ("api" :> (SignInAPI :<|> ModuleAPI)) :<|> StaticAPI

api :: Proxy API
api = Proxy

server :: AppConfig -> Server API
server config@AppConfig{..} =
  f (signInServer :<|> moduleServer) :<|> rawServer _appConfigStaticRoot
  where f = enter (Nat $ runApp config)

loadDb :: AppConfig -> IO ()
loadDb config =
  runSqlPool doMigrations $ _appConfigSqlPool config

app :: AppConfig -> Application
app = serve api . server

runAppConfig :: AppConfig -> IO ()
runAppConfig config = do
  loadDb config
  run 8081 . logStdoutDev . app $ config

main :: IO ()
main = runAppConfig =<< defaultAppConfig "."
