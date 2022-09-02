{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.News where

import Data.Text
import Servant

type NewsAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

news :: Server NewsAPI
news = return "GET categories endpoint" :<|> return "POST categories endpoint"
