{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Config where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.Beam.Postgres

data AppConfig = AppConfig {serverConfig :: ServerConfig, postgresConfig :: PostgresConfig}
  deriving (Show)

newtype ServerConfig = ServerConfig
  { port :: Int
  }
  deriving (Show)

data PostgresConfig = PostgresConfig
  { pgHost :: String,
    pgPort :: Int,
    pgUser :: String,
    pgPassword :: String,
    pgDatabaseName :: String
  }
  deriving (Show)

readConfigFromFile :: (MonadIO m, MonadFail m) => FilePath -> m AppConfig
readConfigFromFile cfgPath =
  liftIO $ C.load [C.Required cfgPath] >>= toAppConfig

toAppConfig :: C.Config -> IO AppConfig
toAppConfig cfg = do
  serverConfig <- toServerConfig $ C.subconfig "API" cfg
  postgresConfig <- toPostgresConfig $ C.subconfig "Database" cfg
  return $ AppConfig {..}

toServerConfig :: C.Config -> IO ServerConfig
toServerConfig cfg = do
  port <- C.require cfg "port"
  return $ ServerConfig {..}

toPostgresConfig :: C.Config -> IO PostgresConfig
toPostgresConfig cfg = do
  pgHost <- C.require cfg "host"
  pgPort <- C.require cfg "port"
  pgUser <- C.require cfg "dbUser"
  pgPassword <- C.require cfg "dbPassword"
  pgDatabaseName <- C.require cfg "dbName"
  return $ PostgresConfig {..}

toPostgresConnectInfo :: PostgresConfig -> ConnectInfo
toPostgresConnectInfo pgConf =
  ConnectInfo
    { connectHost = pgHost pgConf,
      connectPort = fromIntegral $ pgPort pgConf,
      connectUser = pgUser pgConf,
      connectPassword = pgPassword pgConf,
      connectDatabase = pgDatabaseName pgConf
    }
