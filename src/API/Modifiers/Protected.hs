{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Protected where

import App.Monad
import Control.Monad (when)
import qualified DB
import Data.CaseInsensitive as CI (original)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Extended as T
import qualified Effects.Log as Log (logInfo, logWarning)
import Entities.User
import GHC.TypeLits
import Network.Wai
import Servant
import Servant.Docs.Internal as Docs
import Servant.Server.Experimental.Auth
import Servant.Server.Internal.BasicAuth

type instance AuthServerData (AuthProtect "basic-auth") = User

lookupAccount :: BasicAuthData -> App User
lookupAccount basicAuthData = do
  doLogAuthAttempt
  maybeUser <- lookupUserLogin (DB._newsUsers DB.newsDB) username
  maybe onUserNotFound onUserFound maybeUser
  where
    username = decodeUtf8 (basicAuthUsername basicAuthData)
    pass = basicAuthPassword basicAuthData
    onWrongPassword = doLogWrongPassword >> throwError err404
    onAuthorised user = doLogSuccess >> pure user
    onUserNotFound = doLogNoSuchUser >> throwError err404
    onUserFound user =
      if checkPassword pass user
        then onAuthorised user
        else onWrongPassword
    doLogAuthAttempt = Log.logInfo $ "Auth: attempt for User \"" <> username <> "\""
    doLogNoSuchUser = Log.logWarning $ "Auth: User \"" <> username <> "\" not found"
    doLogWrongPassword =
      Log.logWarning $
        "Auth: User \"" <> username <> "\" entered wrong password"
    doLogSuccess =
      Log.logInfo $ "Auth: User \"" <> username <> "\" successfully authorised"

data Protected pType

type AuthHandlerType pType =
  AuthHandler Request (AuthServerData (AuthProtect (AuthName pType)))

instance
  {-# OVERLAPPABLE #-}
  ( HasServer api context,
    HasContextEntry context (AuthHandlerType pType),
    (ProtectionType (AuthServerData (AuthProtect (AuthName pType)))),
    ProtectionType pType
  ) =>
  HasServer (Protected pType :> api) context
  where
  type ServerT (Protected pType :> api) m = AuthServerData (AuthProtect (AuthName pType)) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context delayed =
    route api context (checkUserStatus <$> delayed)
    where
      api = Proxy :: Proxy (AuthProtect (AuthName pType) :> api)
      checkUserStatus f usr = do
        f usr

instance
  {-# OVERLAPPING #-}
  ( HasServer api context,
    HasContextEntry context (AuthHandlerType OptionalAuthorUser)
  ) =>
  HasServer (Protected OptionalAuthorUser :> api) context
  where
  type ServerT (Protected OptionalAuthorUser :> api) m = AuthServerData (AuthProtect (AuthName OptionalAuthorUser)) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context delayed =
    route api context (checkUserStatus <$> delayed)
    where
      api = Proxy :: Proxy (AuthProtect (AuthName OptionalAuthorUser) :> api)
      checkUserStatus f usr = do
        f usr

instance Docs.ToAuthInfo Protected where
  toAuthInfo _ =
    Docs.DocAuthentication
      "Basic Access Authentication"
      "HTTP header \"Authorization: Basic <Base64 encoded \'username:password\'>\""

instance
  ( Docs.HasDocs api
  ) =>
  Docs.HasDocs (Protected pType :> api)
  where
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy api) (endpoint, action')
    where
      authProxy = Proxy :: Proxy Protected
      action' =
        action
          { Docs._authInfo = Docs._authInfo action <> [toAuthInfo authProxy]
          }

newtype AnyUser = AnyUser {getAnyUser :: User}

newtype AdminUser = AdminUser {getAdminUser :: User}

newtype AuthorUser = AuthorUser {getAuthorUser :: User}

newtype OptionalAuthorUser = OptionalAuthorUser {getOptionalAuthorUser :: Maybe User}

type instance AuthServerData (AuthProtect "any-user") = AnyUser

type instance AuthServerData (AuthProtect "admin") = AdminUser

type instance AuthServerData (AuthProtect "author") = AuthorUser

type instance AuthServerData (AuthProtect "optional-author") = OptionalAuthorUser

type family AuthName typ :: Symbol where
  AuthName AnyUser = "any-user"
  AuthName AdminUser = "admin"
  AuthName AuthorUser = "author"
  AuthName OptionalAuthorUser = "optional-author"

class ProtectionType typ where
  cons :: Proxy typ -> User -> typ
  checkUser :: Proxy typ -> User -> Bool
  getUser :: typ -> User

instance ProtectionType AnyUser where
  cons _ = AnyUser
  checkUser _ _ = True
  getUser = getAnyUser

instance ProtectionType AdminUser where
  cons _ = AdminUser
  checkUser _ = _userIsAdmin
  getUser = getAdminUser

instance ProtectionType AuthorUser where
  cons _ = AuthorUser
  checkUser _ u = _userIsAllowedToPost u || _userIsAdmin u
  getUser = getAuthorUser

type AvailableAuthHandlers =
  '[ AuthHandler Request AnyUser,
     AuthHandler Request AdminUser,
     AuthHandler Request AuthorUser,
     AuthHandler Request OptionalAuthorUser
   ]

authHandlerAny :: AppEnv -> AuthHandler Request AnyUser
authHandlerAny = authHandlerBuilder (Proxy :: Proxy AnyUser)

authHandlerAdmin :: AppEnv -> AuthHandler Request AdminUser
authHandlerAdmin = authHandlerBuilder (Proxy :: Proxy AdminUser)

authHandlerAuthor :: AppEnv -> AuthHandler Request AuthorUser
authHandlerAuthor = authHandlerBuilder (Proxy :: Proxy AuthorUser)

authHandlerBuilder :: (ProtectionType typ) => Proxy typ -> AppEnv -> AuthHandler Request typ
authHandlerBuilder prox env = mkAuthHandler handler
  where
    validate pathText ba = do
      usr <- lookupAccount ba
      if checkUser prox usr
        then pure (cons prox usr)
        else doOnUnauthorised pathText usr
    handler req = do
      let maybeBasicAuthData = decodeBAHdr req
      let pathText = T.tshow $ requestMethod req <> " " <> rawPathInfo req
      maybe (throwError err404) (appToHandler env . validate pathText) maybeBasicAuthData
    creatorLogin usr = CI.original (_userLogin usr)
    doLogUnauthorised pathText usr =
      Log.logWarning $
        "User \"" <> creatorLogin usr <> "\" is not authorised to access " <> pathText <> " route"
    doOnUnauthorised pathText usr = doLogUnauthorised pathText usr >> throwError err401

authHandlerOptionalAuthor :: AppEnv -> AuthHandler Request OptionalAuthorUser
authHandlerOptionalAuthor env = mkAuthHandler handler
  where
    validate pathText ba = do
      usr <- lookupAccount ba
      when (_userIsAllowedToPost usr || _userIsAdmin usr) (doLogAuthorised pathText usr)
      pure (OptionalAuthorUser $ Just usr)
    handler req = do
      let maybeBasicAuthData = decodeBAHdr req
      let pathText = T.tshow $ requestMethod req <> " " <> rawPathInfo req
      maybe (pure $ OptionalAuthorUser Nothing) (appToHandler env . validate pathText) maybeBasicAuthData
    creatorLogin usr = CI.original (_userLogin usr)
    doLogAuthorised pathText usr =
      Log.logInfo $
        "User \"" <> creatorLogin usr <> "\" logged to " <> pathText <> " route"

authContext ::
  AppEnv ->
  Context
    '[ AuthHandler Request AnyUser,
       AuthHandler Request AdminUser,
       AuthHandler Request AuthorUser,
       AuthHandler Request OptionalAuthorUser
     ]
authContext env =
  authHandlerAny env
    :. authHandlerAdmin env
    :. authHandlerAuthor env
    :. authHandlerOptionalAuthor env
    :. EmptyContext

