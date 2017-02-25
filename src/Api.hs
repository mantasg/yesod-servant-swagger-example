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

module Api where

import Data.Text        (Text)
import Servant          hiding (Handler)
import Yesod
import Data.Swagger hiding (get)
import Database.Persist.Sqlite
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import GHC.Int (Int64)
import Control.Lens
import Servant.Swagger

import Database
import Model
import AppM


--- Transformer Stack to DB access
readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (readerToEither cfg) server


---

type PersonAPI =  GetEntities
             :<|> GetEntity
             :<|> Echo
             :<|> ProcessRequest
             :<|> WithHeader
             :<|> WithError
             :<|> ReturnHeader
             :<|> AddCar
             :<|> GetCars
             :<|> GetCar


server :: ServerT PersonAPI AppM
server = getEntities
    :<|> getEntity
    :<|> echo
    :<|> processRequest
    :<|> withHeader
    :<|> failingHandler
    :<|> responseHeader
    :<|> addCar
    :<|> getCars
    :<|> getCar


type AddCar = "car" :> "add" :> Get '[PlainText] String
addCar :: AppM String
addCar = do
  _ <- runDb $ insert $ Car "Foo"
  return "foo"

type GetCars = "car"  :> "list" :> Get '[JSON] [Car]
getCars :: AppM [Car]
getCars = runDb $ do
    b <- selectList [] [Asc CarMake]
    return $ map entityVal b

type GetCar = "car" :> "get" :> Capture "id" Int64 :> Get '[JSON] (Maybe Car)
getCar :: Int64 -> AppM (Maybe Car)
getCar i = runDb $ get (toSqlKey i)


-- Request handlers
type GetEntities = "entity" :> "list"  :> Get '[JSON] [Entity']
getEntities :: AppM [Entity']
getEntities = return [ Entity' 1 "One" ]
---
type GetEntity =  "entity" :>  "get"  :> Capture "id" Int
                                      :> Capture "name" String
                                      :> Get '[JSON] Entity'
getEntity :: Int -> String -> AppM Entity'
getEntity i username = return $ Entity' i username
---
type Echo = "echo"        :> QueryParam "text" Text
                          :> Get '[PlainText] String
echo :: Maybe Text -> AppM String
echo = return . show
---
type ProcessRequest = "process-request"   :> ReqBody '[JSON] SampleRequest
                                          :> Post '[PlainText] String
processRequest :: SampleRequest -> AppM String
processRequest = return . field1
---
type WithHeader = "with-header"       :> Servant.Header "Header" String
                                      :> Get '[PlainText] String
withHeader :: Maybe String -> AppM String
withHeader = return . show
---
type WithError = "with-error"        :> Get '[PlainText] String
failingHandler :: AppM String
failingHandler = throwError $ err401 { errBody = "Sorry dear user." }
---
type ReturnHeader = "return-header"     :> Get '[PlainText] (Headers '[Servant.Header "SomeHeader" String] String)
responseHeader :: AppM (Headers '[Servant.Header "SomeHeader" String] String)
responseHeader = return $ Servant.addHeader "headerVal" "foo"



--- Swagger Docs
getSwagger :: Swagger
getSwagger = toSwagger (Proxy :: Proxy PersonAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]
