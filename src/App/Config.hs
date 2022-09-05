{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Config where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

newtype AppConfig = AppConfig {serverConfig :: ServerConfig}
  deriving (Show)

newtype ServerConfig = ServerConfig
  { port :: Int
  }
  deriving (Show)

readConfigFromFile :: (MonadIO m, MonadFail m) => FilePath -> m AppConfig
readConfigFromFile cfgPath =
  liftIO $ C.load [C.Required cfgPath] >>= toAppConfig

toAppConfig :: C.Config -> IO AppConfig
toAppConfig cfg = do
  serverConfig <- toServerConfig $ C.subconfig "API" cfg
  return $ AppConfig {..}

toServerConfig :: C.Config -> IO ServerConfig
toServerConfig cfg = do
  port <- C.require cfg "port"
  return $ ServerConfig {..}
