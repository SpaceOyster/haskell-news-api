{-# LANGUAGE DeriveAnyClass #-}

module App.Error where

import Control.Monad.Catch
  ( Exception,
    MonadCatch (catch),
    MonadThrow (throwM),
  )
import Data.Text

data AppError
  = ConfigError Text
  | DBError Text
  | LoggerError Text
  | APIError Text
  deriving (Show, Exception)

configError :: Text -> AppError
configError = ConfigError

dbError :: Text -> AppError
dbError = DBError

loggerError :: Text -> AppError
loggerError = LoggerError

apiError :: Text -> AppError
apiError = APIError
