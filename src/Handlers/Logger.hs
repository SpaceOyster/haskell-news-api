{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Logger
  ( module Handlers.Logger.Internal,
    LoggerConfig (..),
    withHandle,
    withHandlePure,
  )
where

import Control.Monad (when)
import Data.Configurator as C
import Data.Configurator.Types as C
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import qualified Data.Text as T (Text)
import qualified Effects.Log as Log (Priority (..), Verbosity)
import qualified Handlers.Logger.File as Logger.File
import Handlers.Logger.Internal
import qualified Handlers.Logger.StdOut as Logger.StdOut

data LoggerConfig = LoggerConfig
  { file :: Maybe FilePath,
    verbosity :: Log.Verbosity
  }
  deriving (Show)

fromConfig :: C.Config -> IO LoggerConfig
fromConfig cfg = do
  file <- C.lookup cfg "file"
  verbosity <- C.lookupDefault Log.Info cfg "verbosity"
  return $ LoggerConfig {..}

withHandle :: LoggerConfig -> (Handle -> IO ()) -> IO ()
withHandle LoggerConfig {..} = case file of
  Just f -> Logger.File.withHandle verbosity f
  Nothing -> Logger.StdOut.withHandle verbosity

withHandlePure :: LoggerConfig -> (Handle -> IO a) -> IO (a, [(Log.Priority, T.Text)])
withHandlePure LoggerConfig {..} io = do
  logRef <- newIORef []
  let logAction p t =
        when (p >= verbosity) $
          atomicModifyIORef logRef $
            \l -> ((p, t) : l, ())
  x <- io $ Handle {getLog = logAction}
  logMsgs <- readIORef logRef
  pure (x, reverse logMsgs)
