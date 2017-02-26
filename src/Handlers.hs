{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Handlers where

import Data.Text        (Text)
import Servant          hiding (Handler)
import Yesod
import Database.Persist.Sqlite
import GHC.Int (Int64)

import AppM
import Database
import Model


type AddCar = "car" :> "add" :> Get '[PlainText] String
addCar :: AppM String
addCar = do
  _ <- runDb $ insert $ Car "Foo"
  return "foo"

type GetCars = "car"  :> "list" :> Get '[JSON] [CarModel]
getCars :: AppM [CarModel]
getCars = runDb $ do
    b <- selectList [] [Desc CarId]
    return $ map carFromEntity b

type GetCar = "car" :> "get" :> Capture "id" Int64 :> Get '[JSON] CarModel
getCarModel :: Int64 -> AppM CarModel
getCarModel i = do
  entity <- runDb $ selectList [CarId <-. [(toSqlKey i :: Key Car)]] []
  case entity of
        (x:_) -> return $ carFromEntity x
        []    -> throwError err404  { errBody = "Car not found" }


type GetPersons = "person"  :> "list" :> Get '[JSON] [PersonModel]
getPersons :: AppM [PersonModel]
getPersons = runDb $ do
  list <- selectList [] [Asc PersonId]
  return $ map personFromEntity list



type CaseError = "get1" :> Capture "str" String :> Get '[PlainText] String
caseError :: String -> AppM String
caseError str = do
  case str of
    "404" -> throwError err404
    "401" -> throwError err401
    "500" -> throwError err500
    _       -> return str


type AddJob = "job" :> "add" :> Get '[PlainText] String
addJob :: AppM String
addJob = runDb $ do
  key <- insert $ Job "foo" Nothing
  return $ show $ fromSqlKey key


type GetJobs = "job" :> "list" :> Get '[JSON] [JobModel]
getJobs :: AppM [JobModel]
getJobs = runDb $ do
  list <- selectList [] [Asc JobId]
  return $ map jobFromEntity list



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
