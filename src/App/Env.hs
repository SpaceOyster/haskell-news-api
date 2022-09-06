{-# LANGUAGE KindSignatures #-}

module App.Env where

import Data.Kind (Type)
import Database.Beam.Postgres
import Handlers.Database as DB

data Env (m :: Type -> Type) = Env {dbHandle :: DB.Handle}
