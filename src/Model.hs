{-# LANGUAGE DeriveGeneric #-}

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

data CarModel = CarModel { carId :: Int64, make :: String  } deriving (Generic)
instance ToSchema CarModel
instance ToJSON CarModel

carFromEntity :: Entity Car -> CarModel
carFromEntity entity = let key = entityKey entity
                           value = entityVal entity
                       in  CarModel (fromSqlKey key) (carMake value)


data PersonModel = PersonModel { personId :: Int64
                               , personName1 :: String
                               , personCarId1 :: Maybe Int64
                               } deriving Generic

instance ToSchema PersonModel
instance ToJSON PersonModel

personFromEntity :: Entity Person -> PersonModel
personFromEntity entity = let key = entityKey entity
                              value = entityVal entity
                          in  PersonModel { personId = fromSqlKey key
                                          , personName1 = personName value
                                          , personCarId1 = fromSqlKey <$> personCarId value
                                          } 
