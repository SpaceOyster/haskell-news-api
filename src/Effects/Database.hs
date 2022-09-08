{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Database where

import Control.Monad.Trans (MonadTrans, lift)
import Database.Beam
import Database.Beam.Postgres

class Monad m => MonadDatabase m where
  runQuery :: forall a. (FromBackendRow Postgres a) => Pg a -> m a

instance
  {-# OVERLAPPABLE #-}
  (MonadDatabase m, MonadTrans t, Monad (t m)) =>
  MonadDatabase (t m)
  where
  runQuery = lift . runQuery
