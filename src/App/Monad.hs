{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Monad where

import App.Env (Env (envConfig, envDatabase, envLogger))
import App.Error (AppError, catchRethrowWith, dbError, loggerError)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow, SomeException)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Text.Extended as T
import Effects.Config as Config
import Effects.Database as DB
import Effects.Log as Log
import qualified Handlers.Database as DB
import Handlers.Logger as Logger
import Servant (Handler, ServerError, err503, runHandler)

type AppEnv = Env App

newtype App a = App
  { unApp :: ReaderT AppEnv Handler a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader AppEnv,
      MonadError ServerError
    )

runApp :: App a -> AppEnv -> IO (Either ServerError a)
runApp app = runHandler . flip appToHandler app

appToHandler :: AppEnv -> App a -> Handler a
appToHandler env app = flip runReaderT env . unApp $ (app `catch` onError)
  where
    onError err = do
      logError $ "UNHANDLED ERROR: " <> T.tshow (err :: AppError)
      throwError err503

instance Log.MonadLog App where
  doLog p t = do
    hLog <- asks envLogger
    let logAction = Logger.getLog hLog
    catchRethrowWith rethrow $ App . liftIO $ logAction p t
    where
      rethrow e =
        loggerError . mconcat $
          [ "Logger Error: ",
            T.tshow (e :: SomeException)
          ]

instance DB.MonadDatabase App where
  runQuery q = do
    hDB <- asks envDatabase
    let runQ = DB.runDBQuery hDB
    catchRethrowWith rethrow $ App . liftIO $ runQ q
    where
      rethrow e =
        dbError $
          "Failed to runquery with Error: "
            <> T.tshow (e :: SomeException)

instance Config.MonadConfig App where
  getConfig = asks envConfig
