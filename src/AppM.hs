module AppM where

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Database                   (Config)
import           Servant                    (ServantErr)

type AppM = ReaderT Config (ExceptT ServantErr IO)