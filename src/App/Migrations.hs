module App.Migrations where

import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration as M
import qualified Effects.Log as Log (Priority (..))
import Handlers.Logger as Logger

migrationsList :: [MigrationCommand]
migrationsList =
  [ M.MigrationInitialization,
    M.MigrationDirectory "postgresql"
  ]

mkMigrationOptions :: Logger.Handle -> MigrationOptions
mkMigrationOptions hLog =
  defaultOptions
    { M.optVerbose = M.Verbose,
      M.optLogWriter = migrationLogFunction hLog
    }

migrationLogFunction :: Logger.Handle -> Either Text Text -> IO ()
migrationLogFunction hLog msgE = case msgE of
  Left msg -> getLog hLog Log.Error msg
  Right msg -> getLog hLog Log.Info msg

initiateDBStructure :: Logger.Handle -> Connection -> IO (MigrationResult String)
initiateDBStructure hLog con = do
  let myMigrationOptions = mkMigrationOptions hLog
   in M.runMigrations con myMigrationOptions migrationsList
