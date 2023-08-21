{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Modifiers.Protected where

import App.Monad
import DB
import Data.CaseInsensitive as CI (mk)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Extended as T
import Database.Beam
import qualified Effects.Database as DB
import Effects.Log as Log (MonadLog, logInfo, logWarning)
import Entities.User
import Network.Wai
import Servant
import Servant.Docs.Internal as Docs
import Servant.Server.Experimental.Auth
import Servant.Server.Internal.BasicAuth

type instance AuthServerData (AuthProtect "basic-auth") = User

lookupAccount :: BasicAuthData -> App User
lookupAccount basicAuthData = do
  doLogAuthAttempt
  maybeUser <- lookupUserLogin (_newsUsers newsDB) username
  maybe onUserNotFound onUserFound maybeUser
  where
    username = decodeUtf8 (basicAuthUsername basicAuthData)
    pass = basicAuthPassword basicAuthData
    onUnauthorised = doLogUnauthorised >> throwError err404
    onAuthorised user = doLogSuccess >> pure user
    onUserNotFound = doLogNoSuchUser >> throwError err404
    onUserFound user =
      if checkPassword pass user
        then onAuthorised user
        else onUnauthorised
    doLogAuthAttempt = Log.logInfo $ "Auth: attempt for User \"" <> username <> "\""
    doLogNoSuchUser = Log.logWarning $ "Auth: User \"" <> username <> "\" not found"
    doLogUnauthorised =
      Log.logWarning $
        "Auth: User \"" <> username <> "\" is not authorised to create a new category"
    doLogSuccess =
      Log.logInfo $ "Auth: User \"" <> username <> "\" successfully authorised"

authHandler :: AppEnv -> AuthHandler Request User
authHandler env = mkAuthHandler handler
  where
    handler req = do
      let maybeBasicAuthData = decodeBAHdr req
      maybe (throwError err404) (appToHandler env . lookupAccount) maybeBasicAuthData


data Protected

-- | Basic Authentication
instance
  ( HasServer api context,
    HasContextEntry context (AuthHandler Request User)
  ) =>
  HasServer (Protected :> api) context
  where
  type ServerT (Protected :> api) m = User -> ServerT api m

  route Proxy = route (Proxy :: Proxy (AuthProtect "basic-auth" :> api))

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance Docs.ToAuthInfo Protected where
  toAuthInfo _ =
    Docs.DocAuthentication
      "Basic Access Authentication"
      "HTTP header \"Authorization: Basic <Base64 encoded \'username:password\'>\""

instance
  ( Docs.HasDocs api
  ) =>
  Docs.HasDocs (Protected :> api)
  where
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy api) (endpoint, action')
    where
      authProxy = Proxy :: Proxy Protected
      action' =
        action
          { Docs._authInfo = Docs._authInfo action <> [toAuthInfo authProxy]
          }
