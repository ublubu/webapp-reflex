{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Code where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.Persist.Sql (SqlBackend(..), runMigration)
import Database.Persist.TH (share, mkPersist, sqlSettings,
                            mkMigrate, persistLowerCase)
import GHC.Generics (Generic)

-- TODO: rename "Code" to "Template"?
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Code
  guid Text
  authorId Text
  title Text
  description Text Maybe
  code Text
  Guid guid
  deriving Eq Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll
