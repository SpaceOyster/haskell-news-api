{-# LANGUAGE KindSignatures #-}

module App.Env where

import Data.Kind (Type)
import Handlers.Database as DB
import Handlers.Logger as Logger

data Env (m :: Type -> Type) = Env
  { envLogger :: Logger.Handle,
    envDatabase :: DB.Handle
  }
