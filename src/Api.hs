{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}



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

readerServer :: Config -> Server CombinedAPI
readerServer cfg = enter (readerToEither cfg) server


type CombinedAPI =  WithHeader
             :<|> ReturnHeader
             :<|> AddCar
             :<|> GetCars
             :<|> GetCar
             :<|> CaseError
             :<|> GetPersons
             :<|> AddJob
             :<|> GetJobs
             :<|> GetJob


server :: ServerT CombinedAPI AppM
server = withHeader
    :<|> responseHeader
    :<|> addCar
    :<|> getCars
    :<|> getCarModel
    :<|> caseError
    :<|> getPersons
    :<|> addJob
    :<|> getJobs
    :<|> getJob



--- Swagger Docs
getSwagger :: Swagger
getSwagger = toSwagger (Proxy :: Proxy CombinedAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]
