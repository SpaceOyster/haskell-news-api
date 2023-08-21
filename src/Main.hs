{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import API (app)
import App.Config
import qualified App.Config as C
import App.Env (Env (Env, envConfig, envDatabase, envLogger))
import App.Error
import App.Monad (AppEnv, runApp)
import Control.Monad.Catch (MonadThrow, SomeException, catch, catchAll)
import Control.Monad.IO.Class
import DB
import Data.Function ((&))
import Data.List (intercalate)
import Effects.Config
import Effects.Database
import Effects.Log as Log
import Entities.User
import Handlers.Database
import Handlers.Logger as Logger
import Network.Wai.Handler.Warp (run)
import qualified System.Environment as E
import qualified System.Exit as Exit (die)

main :: IO ()
main = do
  cfg <- readConfig `catch` configException
  runWithApp cfg `catchAll` uncaughtExceptions
  where
    configException :: AppError -> IO AppConfig
    configException e = Exit.die $ "Failed to read config file:" <> show e
    uncaughtExceptions :: SomeException -> IO ()
    uncaughtExceptions e =
      Exit.die $ "Uncaught Exception: " <> show e <> "\nClosing application."

readConfig :: IO C.AppConfig
readConfig = do
  cfgPath : _xs <- E.getArgs
  C.readConfigFromFile cfgPath

runWithApp :: AppConfig -> IO ()
runWithApp cfg =
  Logger.withHandle (loggerConfig cfg) $ \hLog -> do
    env <- initiateEnv hLog cfg
    let port = cfg & C.serverConfig & C.port
    _ <- runApp addRootUser env
    run port (app env)

initiateEnv :: Logger.Handle -> C.AppConfig -> IO AppEnv
initiateEnv hLog cfg = do
  let envLogger = hLog
      envConfig = cfg
  envDatabase <- newPostgresHandler envLogger $ C.postgresConfig cfg
  return $ Env {..}

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

addRootUser ::
  ( MonadDatabase m,
    MonadConfig m,
    MonadIO m,
    Log.MonadLog m,
    MonadThrow m
  ) =>
  m ()
addRootUser = do
  apiCfg <- apiConfig <$> getConfig
  let rootLogin = rootUser apiCfg
  let rootUserCredentials =
        NewUser
          { _newUserName = rootLogin,
            _newUserLogin = rootLogin,
            _newUserPassword = rootPassword apiCfg,
            _newUserIsAdmin = True,
            _newUserIsAllowedToPost = True
          }
  userExists <- isUserLoginTaken (_newsUsers newsDB) rootLogin
  if userExists
    then Log.logInfo $ "Root user: '" <> rootLogin <> "' already exist, skipping DB insertion"
    else insertNewUser (_newsUsers newsDB) rootUserCredentials
