{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Modifiers.Protected where

import App.Monad
import DB
import Data.CaseInsensitive as CI (mk)
import Data.Text.Encoding (decodeUtf8)
import Database.Beam
import qualified Effects.Database as DB
import Entities.User
import Network.Wai
import Servant
import Servant.Docs.Internal as Docs
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
