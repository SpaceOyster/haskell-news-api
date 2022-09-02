{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Users where

import App.Monad
import Data.Text
import Servant

type UsersAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

users :: ServerT UsersAPI App
users = return "GET categories endpoint" :<|> return "POST categories endpoint"
