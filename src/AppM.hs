module AppM where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Servant          hiding (Handler)
import Database


type AppM = ReaderT Config (ExceptT ServantErr IO)
