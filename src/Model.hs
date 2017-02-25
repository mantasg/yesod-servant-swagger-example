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

module Model where


import Yesod
import GHC.Generics
import Data.Swagger hiding (get)


data Entity' = Entity' { id :: Int, name :: String } deriving (Generic)
instance ToJSON Entity'
instance ToSchema Entity'

data SampleRequest = SampleRequest { field1  :: String, field2 :: Maybe String } deriving (Generic)
instance ToJSON SampleRequest
instance FromJSON SampleRequest
instance ToSchema SampleRequest
