{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Images where

import Data.Text
import Servant

type ImagesAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

images :: Server ImagesAPI
images = return "GET categories endpoint" :<|> return "POST categories endpoint"
