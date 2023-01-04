{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Users where

import API.Modifiers.Paginated
import API.Modifiers.Protected ()
import API.Modifiers.Sortable
import App.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text.Extended as T
import Data.Time.Clock
import Database.Beam
import Effects.Config
import Effects.Database as DB
import Effects.Log as Log
import Entities.User
import Servant

type UsersAPI =
  Paginated
    :> SortableBy
         '[ "name",
            "login",
            "registration-date",
            "is-admin",
            "is-allowed-to-post"
          ]
         ('Ascend "name")
    :> Get '[JSON] [UserListItem]
    :<|> AuthProtect "basic-auth" :> ReqBody '[JSON] NewUserJSON :> PostCreated '[JSON] T.Text

users :: ServerT UsersAPI App
users = listUsers :<|> addNewUser

newtype UserListItem = UserListItem User

instance A.ToJSON UserListItem where
  toJSON (UserListItem User {..}) =
    A.object
      [ "name" A..= _userName,
        "login" A..= CI.original _userLogin,
        "registration-date" A..= _userRegistrationDate,
        "is-admin" A..= _userIsAdmin,
        "is-allowed-to-post" A..= _userIsAllowedToPost
      ]

listUsers ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadConfig m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  Pagination ->
  Sorting (CI T.Text) ->
  m [UserListItem]
listUsers p@Pagination {..} sorting = do
  Log.logInfo $ "Get /user sort-by=" <> T.tshow sorting
  usrs <-
    DB.runQuery
      . runSelectReturningList
      . select
      . limit_ limit
      . offset_ offset
      . sortBy_ sorting sorters
      . all_
      $ _newsUsers newsDB
  pure $ UserListItem <$> usrs
  where
    sorters User {..} =
      [ sorterFor_ "name" _userName,
        sorterFor_ "login" _userLogin,
        sorterFor_ "registration-Date" _userRegistrationDate,
        sorterFor_ "is-admin" _userIsAdmin,
        sorterFor_ "is-allowed-to-post" _userIsAllowedToPost
      ]

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
