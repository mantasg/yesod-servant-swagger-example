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

import Servant          hiding (Handler)
import Data.Swagger hiding (get)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Lens
import Servant.Swagger

import Database
import AppM
import Handlers

--- Transformer Stack to DB access
readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (readerToEither cfg) server


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



--- Swagger Docs
getSwagger :: Swagger
getSwagger = toSwagger (Proxy :: Proxy PersonAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]
