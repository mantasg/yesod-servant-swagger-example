{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE EmptyDataDecls      #-}

module Database where

import           Yesod
import           Database.Persist.Sqlite
import           Database.Persist.Postgresql
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Pool
import           Control.Monad.Reader

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
   
   
makePostgresPool :: IO (Pool SqlBackend)
makePostgresPool = do
  p <- runStderrLoggingT $ createPostgresqlPool "host=localhost dbname=postgres user=postgres password=postgres port=5432" 10
  --runSqlPool (runMigration migrateAll) p
  return p
     

   
