{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts      #-}

module Database where

import           Yesod
import           GHC.Generics
import           Data.Swagger hiding (get)
import           Database.Persist.Sqlite
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Trans.Reader
import           Data.Pool
import           Control.Monad.Reader

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Car json
    Id sql=custom_id
    make String
    deriving Generic
|]

instance ToSchema Car

newtype Config = Config { getPool :: Pool SqlBackend }

runDb :: (MonadBaseControl IO m, MonadReader Config m) => ReaderT SqlBackend m b -> m b
runDb query = do
   pool <- Control.Monad.Reader.asks getPool
   runSqlPool query pool

makeSqlitePool :: IO (Pool SqlBackend)
makeSqlitePool = do
   p <- runStderrLoggingT $ createSqlitePool ":memory:" 10
   runSqlPool (runMigration migrateAll) p
   return p
