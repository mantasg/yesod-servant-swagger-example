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
import Data.Aeson

import Database
import AppM
import Handlers

--- Transformer Stack to DB access
readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server CombinedAPI
readerServer cfg = enter (readerToEither cfg) server
---

type CarAPI = AddCar
         :<|> GetCars
         :<|> GetCar
         
carApi :: ServerT CarAPI AppM
carApi = addCar
    :<|> getCars
    :<|> getCarModel
---

type PersonAPI = GetPersons
personApi :: ServerT PersonAPI AppM
personApi = getPersons
---

type JobAPI = AddJob
         :<|> GetJobs
         :<|> GetJob

jobApi :: ServerT JobAPI AppM
jobApi = addJob
    :<|> getJobs
    :<|> getJob
---

type CombinedAPI = CarAPI  
             :<|> PersonAPI 
             :<|> JobAPI
             :<|> WithHeader
             :<|> ReturnHeader
             :<|> CaseError

server :: ServerT CombinedAPI AppM
server = carApi :<|> personApi :<|> jobApi
    :<|> withHeader
    :<|> responseHeader
    :<|> caseError

--- Swagger Docs
getSwagger :: Swagger
getSwagger = toSwagger (Proxy :: Proxy CombinedAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]
