{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Users where

import API.Modifiers.Paginated
import API.Modifiers.Protected ()
import App.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Time.Clock
import Database.Beam
import Effects.Config
import Effects.Database as DB
import Effects.Log as Log
import Entities.User
import Servant

type UsersAPI =
  Paginated :> Get '[JSON] [UserListItem]
    :<|> AuthProtect "basic-auth" :> ReqBody '[JSON] NewUserJSON :> PostCreated '[JSON] T.Text

users :: ServerT UsersAPI App
users = listUsers :<|> addNewUser

data UserListItem = UserListItem
  { _userLIName :: T.Text,
    _userLILogin :: T.Text,
    _userLIRegistrationDate :: UTCTime,
    _userLIIsAdmin :: Bool,
    _userLIIsAllowedToPost :: Bool
  }
  deriving (Show, Generic)

instance A.ToJSON UserListItem where
  toJSON =
    A.genericToJSON
      defaultOptions
        { fieldLabelModifier = A.camelTo2 '-' . drop 7
        }

listUsers ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadConfig m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  Pagination ->
  m [UserListItem]
listUsers p@Pagination {..} = do
  usrs <-
    DB.runQuery
      . runSelectReturningList
      . select
      . limit_ limit
      . offset_ offset
      . orderBy_ (asc_ . _userName)
      . all_
      $ _newsUsers newsDB
  pure $ userToListItem <$> usrs

userToListItem :: User -> UserListItem
userToListItem usr =
  UserListItem
    { _userLIName = _userName usr,
      _userLILogin = CI.original $ _userLogin usr,
      _userLIRegistrationDate = _userRegistrationDate usr,
      _userLIIsAdmin = _userIsAdmin usr,
      _userLIIsAllowedToPost = _userIsAllowedToPost usr
    }

addNewUser ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  User ->
  NewUserJSON ->
  m T.Text
addNewUser usr (NewUserJSON newUser) =
  if _userIsAdmin usr then do_createUser else do_on_unauthorised
  where
    do_createUser = do
      insertNewUser (_newsUsers newsDB) newUser
      do_logSuccess
      return $ "User " <> _newUserName newUser <> " successfully created."
    do_on_unauthorised = do_logUnauthorised >> throwError err401
    do_logSuccess =
      Log.logInfo . mconcat $
        [ "User ",
          CI.original (_userLogin usr),
          " created new user :",
          _newUserName newUser
        ]
    do_logUnauthorised =
      Log.logWarning . mconcat $
        [ "User ",
          CI.original (_userLogin usr),
          " is not authorised to create a new user"
        ]

newtype NewUserJSON = NewUserJSON NewUserCredentials

instance A.FromJSON NewUserJSON where
  parseJSON = A.withObject "NewUserJSON" $ \o -> do
    _newUserLogin <- o .: "login"
    _newUserName <- o .:? "name" .!= _newUserLogin
    _newUserPassword <- o .: "password"
    _newUserIsAdmin <- o .:? "is-admin" .!= False
    _newUserIsAllowedToPost <- o .:? "is-allowed-to-post" .!= False
    return $ NewUserJSON $ NewUser {..}
