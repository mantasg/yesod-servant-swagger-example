{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handlers where

import           Data.ByteString.Lazy.Char8 (pack)
import qualified Database.Esqueleto         as E
import           Database.Persist.Sqlite
import           GHC.Int                    (Int64)
import           Servant                    hiding (Handler)

import           AppM
import           Database
import           Model


type GetPersonCars = "person" :> "cars" :> Get '[JSON] [(Int64, Maybe String)]

getPersonCars :: AppM [(Int64, Maybe String)]
getPersonCars = runDb $ do
  rows <- E.select $
               E.from $ \(person `E.LeftOuterJoin` car)  -> do
                 E.on (person E.^. PersonCarId E.==. E.just (car E.^. CarId))
                 return (person E.^. PersonId, E.just (car E.^. CarMake))

  let tuples = map (\(a,b) -> ( (fromSqlKey . E.unValue) a, E.unValue b)  ) rows

  return tuples



instance MimeRender PlainText Int64 where
  mimeRender _ v = pack $ show v

type AddCar =   "car" :> "add"
             :> ReqBody '[JSON] CarModel
             :> Post '[PlainText] Int64

addCar :: CarModel -> AppM Int64
addCar carModel = do
  entity <- runDb $ insert $ toEntity carModel
  return $ fromSqlKey entity

type UpdateCar =   "car" :> "update"
             :> Capture "id" Int64
             :> ReqBody '[JSON] CarModel
             :> Post '[PlainText] NoContent

updateCar :: Int64 -> CarModel -> AppM NoContent
updateCar i carModel = do
  runDb $ update (toSqlKey i) (toUpdate carModel)
  return NoContent


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

type UpdatePerson =    "person" :> "update"
                    :> Capture "id" Int64
                    :> ReqBody '[JSON] PersonModel
                    :> Post '[PlainText] NoContent

updatePerson :: Int64 -> PersonModel -> AppM NoContent
updatePerson i model = do
  runDb $ update (toSqlKey i) (toUpdate model)
  return NoContent


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

type UpdateJob =    "job" :> "update"
                 :> Capture "id" Int64
                 :> ReqBody '[JSON] JobModel
                 :> Post '[PlainText] NoContent

updateJob :: Int64 -> JobModel -> AppM NoContent
updateJob i model = do
  runDb $ update (toSqlKey i) (toUpdate model)
  return NoContent


type GetJobs = "job" :> "list" :> Get '[JSON] [JobModel]
getJobs :: AppM [JobModel]
getJobs = runDb $ do
  list <- selectList [] [Asc JobId]
  return $ map fromEntity list

type GetJob = "job" :> "get" :> Capture "id" Int64 :>  Get '[JSON] JobModel
getJob :: Int64 -> AppM JobModel
getJob i = runDb $ do
    entity <- selectFirst [JobId <-. [toSqlKey i :: Key Job]] []
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
