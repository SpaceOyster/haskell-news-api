{-# LANGUAGE KindSignatures #-}

module App.Env where

import Data.Kind (Type)
import Database.Beam.Postgres

data Env (m :: Type -> Type) = Env {dbConnection :: Connection}
