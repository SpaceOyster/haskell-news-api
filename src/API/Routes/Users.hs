{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Users where

import API.Modifiers.Protected ()
import App.Monad
import Data.Text
import Entities.User
import Servant

type UsersAPI =
  Get '[JSON] Text
    :<|> AuthProtect "basic-auth" :> PostCreated '[JSON] Text

users :: ServerT UsersAPI App
users = return "GET categories endpoint" :<|> onPost

onPost :: (Monad m) => User -> m Text
onPost _usr = return "POST categories endpoint"
