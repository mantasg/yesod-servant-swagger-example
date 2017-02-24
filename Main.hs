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

--import Yesod
import           Network.Wai
import           Yesod.Core.Types
import           Data.Text        (Text)
import           Servant          hiding (Handler)
import           Yesod
import           Yesod.Static
import           GHC.Generics
import           Data.Swagger hiding (get)
import           Servant.Swagger
import           Control.Lens

import           Database.Persist.Sqlite
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except
import           Data.Pool
import           Control.Monad.Reader
import GHC.Int (Int64)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Car json
    Id sql=custom_id
    make String
    deriving Generic
|]

instance ToSchema Car

newtype Config = Config { getPool :: Pool SqlBackend }

runDb :: (MonadBaseControl IO m, MonadReader Config m) => ReaderT SqlBackend m b -> m b
runDb query = do
   pool <- Control.Monad.Reader.asks getPool
   runSqlPool query pool


type AppM = ReaderT Config (ExceptT ServantErr IO)

readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (readerToEither cfg) server




type PersonAPI =  GetEntities
             :<|> GetEntity
             :<|> Echo
             :<|> ProcessRequest
             :<|> WithHeader
             :<|> WithError
             :<|> ReturnHeader
             :<|> AddCar
             :<|> GetCars
             :<|> GetCar


server :: ServerT PersonAPI AppM
server = getEntities
    :<|> getEntity
    :<|> echo
    :<|> processRequest
    :<|> withHeader
    :<|> failingHandler
    :<|> responseHeader
    :<|> addCar
    :<|> getCars
    :<|> getCar


type AddCar = "car" :> "add" :> Get '[PlainText] String
addCar :: AppM String
addCar = do
  _ <- runDb $ insert $ Car "Foo"
  return "foo"

type GetCars = "car"  :> "list" :> Get '[JSON] [Car]
getCars :: AppM [Car]
getCars = runDb $ do
    b <- selectList [] [Asc CarMake]
    return $ map entityVal b

type GetCar = "car" :> "get" :> Capture "id" Int64 :> Get '[JSON] (Maybe Car)
getCar :: Int64 -> AppM (Maybe Car)
getCar i = runDb $ get (toSqlKey i)

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
getSwaggerR = return $ toJSON $ toSwagger (Proxy :: Proxy PersonAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]

makeSqlitePool :: IO (Pool SqlBackend)
makeSqlitePool = do
  p <- runStderrLoggingT $ createSqlitePool ":memory:" 10
  runSqlPool (runMigration migrateAll) p
  return p

main :: IO ()
main = do
  pool <- makeSqlitePool
  let myServer = readerServer (Config pool)
  let api = serve (Proxy :: Proxy PersonAPI) myServer
  static' <- static "static"
  warp 3000 (App (EmbeddedAPI api) static')
