module Main where

import API (app)
import qualified App.Config as C
import App.Env (Env (Env))
import App.Monad
import Control.Monad.Catch (SomeException, catchAll)
import Data.Function
import Data.List (intercalate)
import Network.Wai.Handler.Warp (run)
import qualified System.Environment as E
import qualified System.Exit as Exit (die)

main :: IO ()
main = flip catchAll uncaughtExceptions $ do
  cfg <- getConfig
  let port = cfg & C.serverConfig & C.port
  env <- initiateEnv cfg
  run port (app env)
  where
    uncaughtExceptions :: SomeException -> IO ()
    uncaughtExceptions e =
      Exit.die $ "Uncaught Exception: " <> show e <> "\nClosing application."

getConfig :: IO C.AppConfig
getConfig = do
  cfgPath : _xs <- E.getArgs
  C.readConfigFromFile cfgPath

initiateEnv :: C.AppConfig -> IO AppEnv
initiateEnv _cfg = return Env

usagePrompt :: String
usagePrompt =
  intercalate
    "\n"
    [ "haskell-news-api - simple API for news service",
      mempty,
      "Usage: haskell-news-api FILE",
      mempty,
      "FILE - is a config file."
    ]
