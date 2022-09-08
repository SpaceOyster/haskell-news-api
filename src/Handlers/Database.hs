{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Handlers.Database where

import Data.Text as T
import Database.Beam
import Database.Beam.Postgres
import qualified Effects.Log as Log
import qualified Handlers.Logger as Logger

newtype Handle = Handle
  { runDBQuery :: forall a. (FromBackendRow Postgres a) => Pg a -> IO a
  }

newPostgresHandler :: Logger.Handle -> ConnectInfo -> IO Handle
newPostgresHandler hLog cfg = do
  connection <- connect cfg
  let logAction = Logger.getLog hLog Log.Debug . T.pack
  return $ Handle {runDBQuery = runBeamPostgresDebug logAction connection}
