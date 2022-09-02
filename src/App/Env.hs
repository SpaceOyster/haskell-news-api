{-# LANGUAGE KindSignatures #-}

module App.Env where

import Data.Kind (Type)

data Env (m :: Type -> Type) = Env {}
