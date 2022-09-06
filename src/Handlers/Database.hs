{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Handlers.Database where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Postgres

newtype Handle = Handle
  { runDBQuery :: forall a. (FromBackendRow Postgres a) => Pg a -> IO a
  }

newPostgresHandler :: ConnectInfo -> IO Handle
newPostgresHandler cfg = do
  connection <- connect cfg
  return $ Handle {runDBQuery = runBeamPostgresDebug putStrLn connection}
