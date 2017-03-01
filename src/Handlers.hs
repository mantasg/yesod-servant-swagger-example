{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Handlers where

import Servant          hiding (Handler)
import Yesod
import Database.Persist.Sqlite
import GHC.Int (Int64)

import AppM
import Database
import Model


type AddCar =   "car" :> "add"
             :> ReqBody '[JSON] CarModel
             :> Post '[PlainText] String

addCar :: CarModel -> AppM String
addCar carModel = do
  entity <- runDb $ insert $ toEntity carModel
  return $ show (fromSqlKey entity)

type UpdateCar =   "car" :> "update"
             :> ReqBody '[JSON] CarModel
             :> Post '[PlainText] String

updateCar :: CarModel -> AppM String
updateCar carModel = do
  let key = toKey carModel
  runDb $ update key (toUpdate carModel)
  return $ show (fromSqlKey key)


type DeleteCar = "car" :> "delete"
              :> Capture "id" Int64
              :> Delete '[PlainText] NoContent

deleteCar :: Int64 -> AppM NoContent
deleteCar i = do
  runDb $ delete (toSqlKey i :: Key Car)
  return NoContent



type GetCars = "car"  :> "list" :> Get '[JSON] [CarModel]
getCars :: AppM [CarModel]
getCars = runDb $ do
    b <- selectList [] [Desc CarId]
    return $ map fromEntity b

type GetCar = "car" :> "get" :> Capture "id" Int64 :> Get '[JSON] CarModel
getCarModel :: Int64 -> AppM CarModel
getCarModel i = do
  entity <- runDb $ selectFirst [CarId <-. [toSqlKey i]] []
  case entity of
        (Just x) -> return $ fromEntity x
        Nothing  -> throwError err404  { errBody = "Car not found" }


type AddPerson =  "person" :> "add"
                :> ReqBody '[JSON] PersonModel
                :> Post '[PlainText] String

addPerson :: PersonModel -> AppM String
addPerson model = do
  key <- runDb $ insert $ toEntity  model
  return $ show (fromSqlKey key)

type GetPerson =  "person" :> "get"
               :> Capture "id" Int64
               :> Get '[JSON] PersonModel

getPerson :: Int64 -> AppM PersonModel
getPerson i = do
  entity <- runDb $ selectFirst [PersonId <-. [toSqlKey i :: Key Person]] []
  case entity of
        (Just x) -> return $ fromEntity x
        Nothing  -> throwError err404  { errBody = "Person not found" }


type GetPersons = "person"  :> "list" :> Get '[JSON] [PersonModel]
getPersons :: AppM [PersonModel]
getPersons = runDb $ do
  list <- selectList [] [Asc PersonId]
  return $ map fromEntity list



type CaseError = "get1" :> Capture "str" String :> Get '[PlainText] String
caseError :: String -> AppM String
caseError str = case str of
                  "404" -> throwError err404
                  "401" -> throwError err401
                  "500" -> throwError err500
                  _     -> return str


type AddJob = "job" :> "add" :> ReqBody '[JSON] JobModel :> Post '[PlainText] String
addJob :: JobModel -> AppM String
addJob model = runDb $ do
  key <- insert (toEntity model :: Job)
  return $ show $ fromSqlKey key

type GetJobs = "job" :> "list" :> Get '[JSON] [JobModel]
getJobs :: AppM [JobModel]
getJobs = runDb $ do
  list <- selectList [] [Asc JobId]
  return $ map fromEntity list

type GetJob = "job" :> "get" :> QueryParam "id" Int64 :>  Get '[JSON] JobModel
getJob :: Maybe Int64 -> AppM JobModel
getJob i =  case i of
  Nothing -> throwError err400  { errBody = "Invalid ID" }
  (Just i') -> runDb $ do
    entity <- selectFirst [JobId <-. [toSqlKey i' :: Key Job]] []
    case entity of
      (Just job) -> return $ fromEntity job
      Nothing    -> throwError err404  { errBody = "Job not found" }




-- Request handlers
type WithHeader = "with-header"       :> Servant.Header "Header" String
                                      :> Get '[PlainText] String
withHeader :: Maybe String -> AppM String
withHeader = return . show
---
type ReturnHeader =     "return-header"     
                     :> Get '[PlainText] (Headers '[Servant.Header "SomeHeader" String] String)
                     
responseHeader :: AppM (Headers '[Servant.Header "SomeHeader" String] String)
responseHeader = return $ Servant.addHeader "headerVal" "foo"
