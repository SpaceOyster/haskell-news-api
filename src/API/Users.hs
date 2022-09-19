{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Users where

import App.Monad
import Data.Text
import Entities.User
import Servant

type UsersAPI =
  Get '[JSON] Text
    :<|> BasicAuth "realm" User :> PostCreated '[JSON] Text

users :: ServerT UsersAPI App
users = return "GET categories endpoint" :<|> onPost

onPost :: (Monad m) => User -> m Text
onPost _usr = return "POST categories endpoint"
