{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}


import           Servant          hiding (Handler)
import           Yesod
import           Yesod.Static
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack, pack)
import GHC.Generics
import Data.Maybe


import EmbeddedAPI
import Database
import Api



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

getSwaggerR :: Handler String
getSwaggerR = return $ unpack $ encode getSwagger


data AppConfig = AppConfig { port :: Int, staticDirectory :: String } deriving (Generic, Show)
instance FromJSON AppConfig


defaultAppConfig :: AppConfig 
defaultAppConfig = AppConfig 3000 "static"

getConfig :: IO AppConfig
getConfig = do 
  fileContent <- readFile "config.json"
  let appConfig = decode (pack fileContent) :: Maybe AppConfig
  return $ fromMaybe defaultAppConfig appConfig

  

main :: IO ()
main = do
  appConfig <- getConfig  
  pool <- makeSqlitePool
  --pool <- makePostgresPool
  let myServer = readerServer (Config pool)
  let api = serve (Proxy :: Proxy CombinedAPI) myServer
  static' <- static (staticDirectory appConfig)
  warp (port appConfig) (App (EmbeddedAPI api) static')
