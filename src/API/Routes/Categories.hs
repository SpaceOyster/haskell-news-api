{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Categories where

import App.Monad
import Data.Text
import Entities.Category
import Servant

type CategoriesAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

newtype CategoryJSON = CategoryJSON Category

categories :: ServerT CategoriesAPI App
categories = return "GET categories endpoint" :<|> return "POST categories endpoint"
