{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Categories where

import App.Monad
import Data.Text
import Servant

type CategoriesAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

categories :: ServerT CategoriesAPI App
categories = return "GET categories endpoint" :<|> return "POST categories endpoint"
