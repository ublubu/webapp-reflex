{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Database.Persist.Sql (runSqlPool)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Servant
import Servant.Server

import API.Code
import API.SignIn
import Database.Code
import Server.App
import Server.Code
import Server.SignIn

type StaticAPI = Raw
type API = ("api" :> (SignInAPI :<|> CodeAPI)) :<|> StaticAPI

api :: Proxy API
api = Proxy

server :: AppConfig -> Server API
server config = f (signInServer :<|> codeServer) :<|> serveDirectory (_appConfigStaticRoot config)
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
