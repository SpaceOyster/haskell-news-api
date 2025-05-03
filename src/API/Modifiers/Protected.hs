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

lookupAccount :: BasicAuthData -> App (Maybe User)
lookupAccount basicAuthData = do
  doLogAuthAttempt
  lookupUserLogin (DB._newsUsers DB.newsDB) username
  where
    username = decodeUtf8 (basicAuthUsername basicAuthData)
    doLogAuthAttempt = Log.logInfo $ "Auth: attempt for User \"" <> username <> "\""

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
  (Docs.HasDocs api) =>
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
authHandlerBuilder prox env =
  mkAuthHandler $ \req -> do
    let maybeBasicAuthData = decodeBAHdr req
    let pathText = T.tshow $ requestMethod req <> " " <> rawPathInfo req
    maybe (throwError err404) (appToHandler env . strictAuthHandler prox pathText) maybeBasicAuthData

strictAuthHandler ::
  (ProtectionType typ) =>
  Proxy typ ->
  T.Text ->
  BasicAuthData ->
  App typ
strictAuthHandler prox pathText ba = do
  maybeUser <- lookupAccount ba
  usr <- maybe onUserNotFound onUserFound maybeUser
  if checkUser prox usr
    then pure (cons prox usr)
    else doOnUnauthorised usr
  where
    username = decodeUtf8 (basicAuthUsername ba)
    pass = basicAuthPassword ba
    creatorLogin usr = CI.original (_userLogin usr)
    onUserFound user =
      if checkPassword pass user
        then onAuthorised user
        else onWrongPassword
    doOnUnauthorised usr = doLogUnauthorised usr >> throwError err401
    onUserNotFound = doLogNoSuchUser >> throwError err404
    onAuthorised user = doLogSuccess >> pure user
    onWrongPassword = doLogWrongPassword >> throwError err404
    doLogWrongPassword =
      Log.logWarning $
        "Auth: User \"" <> username <> "\" entered wrong password"
    doLogNoSuchUser = Log.logWarning $ "Auth: User \"" <> username <> "\" not found"
    doLogUnauthorised usr =
      Log.logWarning $
        "User \"" <> creatorLogin usr <> "\" is not authorised to access " <> pathText <> " route"
    doLogSuccess =
      Log.logInfo $ "Auth: User \"" <> username <> "\" successfully authorised"

authHandlerOptionalAuthor :: AppEnv -> AuthHandler Request OptionalAuthorUser
authHandlerOptionalAuthor env =
  mkAuthHandler $ \req -> do
    let maybeBasicAuthData = decodeBAHdr req
    let pathText = T.tshow $ requestMethod req <> " " <> rawPathInfo req
    maybe (pure $ OptionalAuthorUser Nothing) (appToHandler env . optionalAuthHandler pathText) maybeBasicAuthData

optionalAuthHandler ::
  T.Text ->
  BasicAuthData ->
  App OptionalAuthorUser
optionalAuthHandler pathText ba = do
  maybeUser <- lookupAccount ba
  usrM <- maybe onUserNotFound onUserFound maybeUser
  pure $ OptionalAuthorUser usrM
  where
    username = decodeUtf8 (basicAuthUsername ba)
    pass = basicAuthPassword ba
    creatorLogin usr = CI.original (_userLogin usr)
    onUserFound user =
      if checkPassword pass user
        then onAuthorised user
        else onWrongPassword
    onAuthorised user = doLogSuccess >> pure (Just user)
    onWrongPassword = doLogWrongPassword >> pure Nothing
    onUserNotFound = doLogNoSuchUser >> pure Nothing
    doLogWrongPassword =
      Log.logWarning $
        "Auth: User \"" <> username <> "\" entered wrong password"
    doLogNoSuchUser = Log.logWarning $ "Auth: Optional User \"" <> username <> "\" not found, proceeding as unauthenticated"
    doLogSuccess =
      Log.logInfo $ "Auth: Optional User \"" <> username <> "\" successfully authorised"

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
