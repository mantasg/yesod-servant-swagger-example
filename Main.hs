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

--import Yesod
import           Network.Wai
import           Yesod.Core.Types 
import           Data.Text        (Text)
import           Servant          hiding (Handler)
import           Yesod 
import           Yesod.Static
import           GHC.Generics
import           Data.Swagger
import           Servant.Swagger
import           Control.Lens

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Car
    make String
    deriving Show
|]


data Config = Config 
    { getPool :: IO ConnectionPool
    }
    
makePool :: IO ConnectionPool
makePool = runNoLoggingT $ do
  createSqlitePool ":memory:" 10
  
defaultConfig = Config makePool
  
  
  

data Person = Person String deriving Generic
instance ToJSON Person

type AppM = ReaderT Config (ExceptT ServantErr IO)

readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (readerToEither cfg) server




type PersonAPI =  GetEntities

server :: ServerT PersonAPI AppM
server = getEntities

  


-- Model and stuff
data Entity' = Entity' { id :: Int, name :: String } deriving (Generic)
instance ToJSON Entity'
instance ToSchema Entity'

data SampleRequest = SampleRequest { field1  :: String, field2 :: Maybe String } deriving (Generic)
instance ToJSON SampleRequest
instance FromJSON SampleRequest
instance ToSchema SampleRequest


-- Request handlers
type GetEntities = "entity" :> "list"  :> Get '[JSON] [Entity']
getEntities :: AppM [Entity']
getEntities = return [ Entity' 1 "One" ]
---
type GetEntity =  "entity" :>  "get"  :> Capture "id" Int  
                                      :> Capture "name" String
                                      :> Get '[JSON] Entity'                                        
getEntity :: Server GetEntity
getEntity i username = return $ Entity' i username
---                                      
type Echo = "echo"        :> QueryParam "text" Text  
                          :> Get '[PlainText] String
echo :: Server Echo
echo = return . show
---                                    
type ProcessRequest = "process-request"   :> ReqBody '[JSON] SampleRequest
                                          :> Post '[PlainText] String                                 
processRequest :: Server ProcessRequest
processRequest = return . field1
---                                    
type WithHeader = "with-header"       :> Servant.Header "Header" String
                                      :> Get '[PlainText] String
withHeader :: Server WithHeader
withHeader = return . show 
---
type WithError = "with-error"        :> Get '[PlainText] String
failingHandler :: Server WithError
failingHandler = throwError $ err401 { errBody = "Sorry dear user." }
---
type ReturnHeader = "return-header"     :> Get '[PlainText] (Headers '[Servant.Header "SomeHeader" String] String)
responseHeader :: Server ReturnHeader
responseHeader = return $ Servant.addHeader "headerVal" "foo"

-- Servant Bits
type MyAPI =   GetEntity
          :<|> Echo
          :<|> ProcessRequest
          :<|> WithHeader                                  
          :<|> WithError
          :<|> ReturnHeader

myAPIServer :: Server MyAPI
myAPIServer = 
       getEntity
  :<|> echo
  :<|> processRequest
  :<|> withHeader
  :<|> failingHandler
  :<|> responseHeader

-- Servant Yesod bits
newtype EmbeddedAPI = EmbeddedAPI { eapiApplication :: Application }
                                   
instance RenderRoute EmbeddedAPI where
  data Route EmbeddedAPI = EmbeddedAPIR ([Text], [(Text, Text)]) deriving(Eq, Show, Read)
  renderRoute (EmbeddedAPIR t) = t

instance ParseRoute EmbeddedAPI where
  parseRoute t = Just (EmbeddedAPIR t)
  
instance Yesod master => YesodSubDispatch EmbeddedAPI (HandlerT master IO) where
  yesodSubDispatch YesodSubRunnerEnv{..} req = resp
    where
      master = yreSite ysreParentEnv
      site = ysreGetSub master
      resp = eapiApplication site req  
------------------------------------------------
  
data App = App { appAPI :: EmbeddedAPI 
               , getStatic :: Static
               }

instance Yesod App
mkYesod "App" [parseRoutes|
/        HomeR    GET
/swagger SwaggerR GET
/api/    SubsiteR EmbeddedAPI appAPI
/static  StaticR  Static      getStatic
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getSwaggerR :: Handler Value
getSwaggerR = return $ toJSON $ toSwagger (Proxy :: Proxy MyAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]
  
  
main :: IO ()
main = do 
  runSqlite ":memory:" $ do
      runMigration migrateAll  
      insert $ Car "Mazda"
      records <- selectList [] [ Asc CarMake ]
      liftIO $ print records
      
      liftIO $ do 
        let myServer = readerServer defaultConfig
        let api = serve (Proxy :: Proxy PersonAPI) myServer
        --let api = serve (Proxy :: Proxy MyAPI) myAPIServer
        static' <- static "static"
        warp 3000 (App (EmbeddedAPI api) static')       
  
