{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Images where

import App.Monad
import Data.Text
import Servant

type ImagesAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

images :: ServerT ImagesAPI App
images = return "GET categories endpoint" :<|> return "POST categories endpoint"
