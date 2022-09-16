{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Monad where

import App.Env (Env (envConfig, envDatabase, envLogger))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Effects.Config as Config
import Effects.Database as DB
import Effects.Log as Log
import qualified Handlers.Database as DB
import Handlers.Logger as Logger
import Servant (Handler)

type AppEnv = Env App

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader AppEnv
    )

runApp :: App a -> AppEnv -> IO a
runApp app = runReaderT (unApp app)

appToHandler :: AppEnv -> App a -> Handler a
appToHandler env = liftIO . flip runApp env

instance Log.MonadLog App where
  doLog p t = do
    hLog <- asks envLogger
    let logAction = Logger.getLog hLog
    App . liftIO $ logAction p t

instance DB.MonadDatabase App where
  runQuery q = do
    hDB <- asks envDatabase
    let runQ = DB.runDBQuery hDB
    App . liftIO $ runQ q

instance Config.MonadConfig App where
  getConfig = asks envConfig
