{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Users where

import App.Monad
import Data.Text
import Entities.User
import Servant
import Servant.Server.Experimental.Auth

type instance AuthServerData (AuthProtect "basic-auth") = User

type UsersAPI =
  Get '[JSON] Text
    :<|> AuthProtect "basic-auth" :> PostCreated '[JSON] Text

users :: ServerT UsersAPI App
users = return "GET categories endpoint" :<|> onPost

onPost :: (Monad m) => User -> m Text
onPost _usr = return "POST categories endpoint"
