{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Users where

import API.Modifiers.Protected ()
import App.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import DB
import Data.Aeson as A
import Data.CaseInsensitive (original)
import Data.Text
import Effects.Database as DB
import Effects.Log as Log
import Entities.User
import Servant

type UsersAPI =
  Get '[JSON] Text
    :<|> AuthProtect "basic-auth" :> ReqBody '[JSON] NewUserJSON :> PostCreated '[JSON] Text

users :: ServerT UsersAPI App
users = return "GET categories endpoint" :<|> addNewUser

addNewUser ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  User ->
  NewUserJSON ->
  m Text
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
          original (_userLogin usr),
          " created new user :",
          _newUserName newUser
        ]
    do_logUnauthorised =
      Log.logWarning . mconcat $
        [ "User ",
          original (_userLogin usr),
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
