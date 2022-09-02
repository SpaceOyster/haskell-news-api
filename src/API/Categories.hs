{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Categories where

import Data.Text
import Servant

type CategoriesAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

categories :: Server CategoriesAPI
categories = return "GET categories endpoint" :<|> return "POST categories endpoint"
