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
getEntities :: Server GetEntities
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
          :<|> GetEntities
          :<|> Echo
          :<|> ProcessRequest
          :<|> WithHeader                                  
          :<|> WithError
          :<|> ReturnHeader

myAPIServer :: Server MyAPI
myAPIServer = 
       getEntity
  :<|> getEntities 
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
  let api = serve (Proxy :: Proxy MyAPI) myAPIServer
  static' <- static "static"
  warp 3000 (App (EmbeddedAPI api) static') 
  
