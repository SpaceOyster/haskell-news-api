module Handlers.Logger.Internal
  ( Handle (..),
    Log.Priority (..),
    hLogDebug,
    hLogInfo,
    hLogWarning,
    hLogError,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Effects.Log as Log (Priority (..))
import Prelude hiding (log)

newtype Handle = Handle
  { getLog :: Log.Priority -> Text -> IO ()
  }

hLogDebug, hLogInfo, hLogWarning, hLogError :: MonadIO m => Handle -> Text -> m ()
hLogDebug h = liftIO . getLog h Log.Debug
hLogInfo h = liftIO . getLog h Log.Info
hLogWarning h = liftIO . getLog h Log.Warning
hLogError h = liftIO . getLog h Log.Error
