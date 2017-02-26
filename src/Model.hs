{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model where

import Yesod
import GHC.Generics
import Data.Swagger hiding (get)
import GHC.Int
import Database
import Database.Persist.Sqlite

data Entity' = Entity' { id :: Int, name :: String } deriving (Generic)
instance ToJSON Entity'
instance ToSchema Entity'

data SampleRequest = SampleRequest { field1  :: String, field2 :: Maybe String } deriving (Generic)
instance ToJSON SampleRequest
instance FromJSON SampleRequest
instance ToSchema SampleRequest

data CarModel = CarModel { id :: Int64, make :: String  } deriving (Generic)
instance ToSchema CarModel
instance ToJSON CarModel

carFromEntity :: Entity Car -> CarModel
carFromEntity entity = let key = entityKey entity
                           value = entityVal entity
                       in  CarModel (fromSqlKey key) (carMake value)


data PersonModel = PersonModel { id :: Maybe Int64
                               , name :: String
                               , address :: Maybe String
                               , carId :: Maybe Int64
                               } deriving Generic

instance ToSchema PersonModel
instance ToJSON PersonModel

personFromEntity :: Entity Person -> PersonModel
personFromEntity entity = let key = entityKey entity
                              value = entityVal entity
                          in  PersonModel { id = Just $ fromSqlKey key
                                          , name = personName value
                                          , address = personAddress value
                                          , carId = fromSqlKey <$> personCarId value
                                          }


data JobModel = JobModel { id :: Maybe Int64
                         , title :: String
                         , description :: Maybe String
                         } deriving Generic

instance ToSchema JobModel
instance ToJSON JobModel


jobFromEntity :: Entity Job -> JobModel
jobFromEntity entity = let key = entityKey entity
                           value = entityVal entity
                       in JobModel { id = Just $ fromSqlKey key
                                   , title = jobTitle value
                                   , description = jobDescription value
                                   }
