{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Config where

import App.Config
import Control.Monad.Trans (MonadTrans, lift)

class Monad m => MonadConfig m where
  getConfig :: m AppConfig

instance
  {-# OVERLAPPABLE #-}
  (MonadConfig m, MonadTrans t, Monad (t m)) =>
  MonadConfig (t m)
  where
  getConfig = lift getConfig
