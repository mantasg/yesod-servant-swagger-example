{-# LANGUAGE FlexibleInstances #-}

module AppM where

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Database                   (Config)
import           Servant                    (ServantErr, Handler(..), runHandler)
import           Yesod
import Control.Monad.IO.Unlift

type AppM = ReaderT Config Handler

instance MonadUnliftIO Handler where
  askUnliftIO = Handler . ExceptT $ withUnliftIO $ \u ->
    return $ Right $ (UnliftIO (\v -> runHandler v >>= either (error "foo") (unliftIO u . return)))

makeNat :: Config -> AppM x -> Handler x
makeNat = flip runReaderT