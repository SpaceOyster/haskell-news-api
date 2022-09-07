{-# LANGUAGE DeriveAnyClass #-}

module App.Error where

import Control.Monad.Catch (Exception)
import Data.Text

data AppError
  = DBError Text
  | LoggerError Text
  deriving (Show, Exception)

dbError :: Text -> AppError
dbError = DBError

loggerError :: Text -> AppError
loggerError = LoggerError
