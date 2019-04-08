{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Exception          (SomeException, try)
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Data.Maybe
import           GHC.Generics
import           Servant                    hiding (Handler)
import           Yesod
import           Yesod.Static

import           Api
import           Database
import           EmbeddedAPI

data App = App { appAPI    :: EmbeddedAPI
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
  fileContent <- try $ readFile "config.json" :: IO (Either SomeException String)
  case fileContent of
    (Left _)  -> return defaultAppConfig
    (Right c) -> do
      let appConfig = decode (pack c) :: Maybe AppConfig
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
