{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Users where

import Data.Text
import Servant

type UsersAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

users :: Server UsersAPI
users = return "GET categories endpoint" :<|> return "POST categories endpoint"
