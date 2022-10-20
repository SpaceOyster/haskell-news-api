{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Config where

import API.Modifiers.Paginated
import App.Error
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Text.Extended as T
import Database.Beam.Postgres
import Effects.Log as Log
import Handlers.Database
import Handlers.Logger as Logger

data AppConfig = AppConfig
  { loggerConfig :: LoggerConfig,
    serverConfig :: ServerConfig,
    apiConfig :: APIConfig,
    postgresConfig :: PostgresConfig
  }
  deriving (Show)

newtype ServerConfig = ServerConfig
  { port :: Int
  }
  deriving (Show)

data APIConfig = APIConfig
  { rootUser :: Text,
    rootPassword :: Text,
    pagination :: PaginationConfig
  }
  deriving (Show)

readConfigFromFile :: (MonadIO m, MonadCatch m) => FilePath -> m AppConfig
readConfigFromFile cfgPath = flip catch rethrow $ do
  cfgContents <- liftIO (C.load [C.Required cfgPath])
  liftIO $ toAppConfig cfgContents
  where
    rethrow e =
      throwM . configError . mconcat $
        [ "Failed to read config file: ",
          T.tshow cfgPath,
          ". With Error: ",
          T.tshow (e :: C.ConfigError)
        ]

toAppConfig :: C.Config -> IO AppConfig
toAppConfig cfg = do
  loggerConfig <- readSubconfig cfg "logger" toLoggerConfig
  serverConfig <- readSubconfig cfg "server" toServerConfig
  postgresConfig <- readSubconfig cfg "Database" toPostgresConfig
  apiConfig <- readSubconfig cfg "API" toAPIConfig
  return $ AppConfig {..}

readSubconfig :: C.Config -> C.Name -> (C.Config -> IO a) -> IO a
readSubconfig cfg subconf parser = do
  parser (C.subconfig subconf cfg) `catch` rethrow
  where
    rethrow e =
      throwM . configError . mconcat $
        [ "Failed to read subconfig: ",
          T.tshow subconf,
          ". With Error: ",
          T.tshow (e :: C.KeyError)
        ]

toLoggerConfig :: C.Config -> IO LoggerConfig
toLoggerConfig cfg = do
  file <- C.lookup cfg "file"
  verbosity <- C.lookupDefault Log.Info cfg "verbosity"
  return $ LoggerConfig {..}

toServerConfig :: C.Config -> IO ServerConfig
toServerConfig cfg = do
  port <- C.require cfg "port"
  return $ ServerConfig {..}

toAPIConfig :: C.Config -> IO APIConfig
toAPIConfig cfg = do
  rootUser <- C.require cfg "rootUser"
  rootPassword <- C.require cfg "rootPassword"
  defaultOffset <- C.lookupDefault 0 cfg "pagination.offset"
  maxLimit <- C.lookupDefault 100 cfg "pagination.max-limit"
  defaultLimit <- C.lookupDefault maxLimit cfg "pagination.limit"
  let pagination = PaginationConfig {..}
  return $ APIConfig {..}

toPostgresConfig :: C.Config -> IO PostgresConfig
toPostgresConfig cfg = do
  connectHost <- C.require cfg "host"
  connectPort <- C.require cfg "port"
  connectUser <- C.require cfg "dbUser"
  connectPassword <- C.require cfg "dbPassword"
  connectDatabase <- C.require cfg "dbName"
  return $ ConnectInfo {..}
