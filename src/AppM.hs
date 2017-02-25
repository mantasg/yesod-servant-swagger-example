module AppM where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Servant (ServantErr)
import Database (Config)

type AppM = ReaderT Config (ExceptT ServantErr IO)
