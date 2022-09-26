{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module API.Protected where

import App.Env
import App.Monad
import DB
import Data.CaseInsensitive as CI (mk)
import Data.Text.Encoding (decodeUtf8)
import Database.Beam
import qualified Effects.Database as DB
import Entities.User
import qualified Handlers.Database as DB
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Internal.BasicAuth

type instance AuthServerData (AuthProtect "basic-auth") = User

lookupAccount :: BasicAuthData -> App User
lookupAccount basicAuthData = do
  let username = CI.mk $ decodeUtf8 (basicAuthUsername basicAuthData)
  let pass = basicAuthPassword basicAuthData
  maybeUser <-
    DB.runQuery
      . runSelectReturningOne
      . select
      . filter_ (\u -> _userLogin u ==. val_ username)
      $ all_ (_newsUsers newsDB)
  case maybeUser of
    Nothing -> throwError err404
    Just user -> if checkPassword pass user then pure user else throwError err404

authHandler :: AppEnv -> AuthHandler Request User
authHandler env = mkAuthHandler handler
  where
    handler req = do
      let maybeBasicAuthData = decodeBAHdr req
      maybe (throwError err404) (appToHandler env . lookupAccount) maybeBasicAuthData
