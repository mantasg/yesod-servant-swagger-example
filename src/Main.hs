{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


import           Servant          hiding (Handler)
import           Yesod
import           Yesod.Static

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

getSwaggerR :: Handler Value
getSwaggerR = return $ toJSON getSwagger


main :: IO ()
main = do
  pool <- makeSqlitePool
  let myServer = readerServer (Config pool)
  let api = serve (Proxy :: Proxy PersonAPI) myServer
  static' <- static "static"
  warp 3000 (App (EmbeddedAPI api) static')
