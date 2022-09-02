{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Monad where

import App.Env (Env)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
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

appToHandler :: AppEnv -> App a -> Handler a
appToHandler env = liftIO . flip runReaderT env . unApp
