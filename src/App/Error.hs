{-# LANGUAGE DeriveAnyClass #-}

module App.Error where

import Control.Monad.Catch (Exception)
import Data.Text

data AppError
  = ConfigError Text
  | DBError Text
  | LoggerError Text
  deriving (Show, Exception)

configError :: Text -> AppError
configError = ConfigError

dbError :: Text -> AppError
dbError = DBError

loggerError :: Text -> AppError
loggerError = LoggerError
