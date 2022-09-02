{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.News where

import App.Monad
import Data.Text
import Servant

type NewsAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

news :: ServerT NewsAPI App
news = return "GET categories endpoint" :<|> return "POST categories endpoint"
