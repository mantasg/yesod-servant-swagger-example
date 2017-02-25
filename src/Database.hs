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
import           GHC.Generics
import           Data.Swagger hiding (get)
import           Database.Persist.Sqlite
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Pool
import           Control.Monad.Reader

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Car json
    make String
    deriving Generic
|]

instance ToSchema Car

newtype Config = Config { getPool :: Pool SqlBackend }

runDb :: (MonadBaseControl IO m, MonadReader Config m) => ReaderT SqlBackend m b -> m b
runDb query = do
   pool <- asks getPool
   runSqlPool query pool

makeSqlitePool :: IO (Pool SqlBackend)
makeSqlitePool = do
   p <- runStderrLoggingT $ createSqlitePool ":memory:" 10
   runSqlPool (runMigration migrateAll) p
   _ <- runSqlPool (insert $ Car "make1") p
   _ <- runSqlPool (insert $ Car "make2") p
   _ <- runSqlPool (insert $ Car "make3") p
   return p
