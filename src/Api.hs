{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Control.Lens
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Swagger               hiding (get)
import           Servant                    hiding (Handler)
import           Servant.Swagger

import           AppM
import           Database
import           Handlers

--- Transformer Stack to DB access
readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server CombinedAPI
readerServer cfg = enter (readerToEither cfg) server
---

type CarAPI = AddCar
         :<|> UpdateCar
         :<|> GetCars
         :<|> GetCar
         :<|> DeleteCar

carApi :: ServerT CarAPI AppM
carApi = addCar
    :<|> updateCar
    :<|> getCars
    :<|> getCarModel
    :<|> deleteCar
--

type PersonAPI = AddPerson
            :<|> UpdatePerson
            :<|> GetPerson
            :<|> GetPersons
            :<|> GetPersonCars

personApi :: ServerT PersonAPI AppM
personApi = addPerson
       :<|> updatePerson
       :<|> getPerson
       :<|> getPersons
       :<|> getPersonCars
--

type JobAPI = AddJob
         :<|> UpdateJob
         :<|> GetJobs
         :<|> GetJob

jobApi :: ServerT JobAPI AppM
jobApi = addJob
    :<|> updateJob
    :<|> getJobs
    :<|> getJob
--

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

-- Swagger Docs
getSwagger :: Swagger
getSwagger = toSwagger (Proxy :: Proxy CombinedAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]
