{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database where

import           Control.Monad.Logger        (runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Pool
import           Data.Text
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Car json
    make String

  Person json
    name String
    address String Maybe
    carId CarId Maybe

  Job json
    title String
    description String Maybe
    deriving
|]

newtype Config = Config { getPool :: Pool SqlBackend }

runDb :: (MonadBaseControl IO m, MonadReader Config m) => ReaderT SqlBackend m b -> m b
runDb query = do
   pool <- asks getPool
   runSqlPool query pool

makeSqlitePool :: IO (Pool SqlBackend)
makeSqlitePool = do
   p <- runStderrLoggingT $ createSqlitePool ":memory:" 10
   runSqlPool (runMigration migrateAll) p
   carId <- runSqlPool (insert $ Car "make1") p
   _ <- runSqlPool (insert $ Car "make2") p
   _ <- runSqlPool (insert $ Car "make3") p
   _ <- runSqlPool (insert $ Person "George" Nothing (Just carId)) p
   _ <- runSqlPool (insert $ Person "John" (Just "address") (Just carId)) p
   return p

makeSqlitePoolFromFile :: String -> IO (Pool SqlBackend)
makeSqlitePoolFromFile fn = do
  p <- runStderrLoggingT $ createSqlitePool (pack fn) 10
  runSqlPool (runMigration migrateAll) p
  return p

makePostgresPool :: Bool -> IO (Pool SqlBackend)
makePostgresPool doMigrate = do
  p <- runStderrLoggingT $ createPostgresqlPool "host=localhost dbname=postgres user=postgres password=postgres port=5432" 10
  when doMigrate $ runSqlPool (runMigration migrateAll) p
  return p
