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
{-# LANGUAGE TypeFamilies          #-}
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
import           Control.Monad.Trans.Except


-- Model and stuff
data Entity' = Entity' { id :: Int, name :: String } deriving (Generic)
instance ToJSON Entity'
instance ToSchema Entity'

data SampleRequest = SampleRequest { field1  :: String, field2 :: Maybe String } deriving (Generic)
instance ToJSON SampleRequest
instance FromJSON SampleRequest
instance ToSchema SampleRequest

getEntities :: [Entity'] 
getEntities = [ Entity' 1 "One" ]

getEntity :: Int -> String -> Entity' 
getEntity i username = Entity' i username
                    
processRequest :: SampleRequest -> String
processRequest req = field1 req
         
failingHandler :: ExceptT ServantErr IO String
failingHandler = throwError $ err401 { errBody = "Sorry dear user." }

responseHeader :: Headers '[Servant.Header "SomeHeader" String] String
responseHeader = Servant.addHeader "headerVal" "foo"

-- Servant Bits
type MyAPI =   "entity" :>  "get"  :> Capture "id" Int  
                                   :> Capture "name" String
                                   :> Get '[JSON] Entity'  
                        
          :<|> "entity" :> "list"  :> Get '[JSON] [Entity'] 
          
          :<|> "echo"              :> QueryParam "text" Text 
                                   :> Get '[PlainText] String

          :<|> "process-request"   :> ReqBody '[JSON] SampleRequest
                                   :> Post '[PlainText] String 

          :<|> "with-header"       :> Servant.Header "Header" String
                                   :> Get '[PlainText] String
                                                          
          :<|> "with-error"        :> Get '[PlainText] String
          
          :<|> "return-header"     :> Get '[PlainText] (Headers '[Servant.Header "SomeHeader" String] String)                                   



myAPIServer :: Server MyAPI
myAPIServer = 
       (\userId username -> return (getEntity userId username))
  :<|> return getEntities 
  :<|> (\text -> return (show text))
  :<|> (\body -> return (processRequest body))
  :<|> (\header -> return (show header))
  :<|> failingHandler
  :<|> return responseHeader

-- Servant Yesod bits
data EmbeddedAPI = EmbeddedAPI { eapiApplication :: Application }
                                   
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
  
  
main :: IO ()
main = do 
  let api = serve (Proxy :: Proxy MyAPI) myAPIServer
  static' <- static "static"
  warp 3000 (App (EmbeddedAPI api) static') 
  
