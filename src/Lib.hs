{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import API.Categories
import API.Images
import API.News
import API.Users
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type AppAPI =
  "categories" :> CategoriesAPI
    :<|> "images" :> ImagesAPI
    :<|> "news" :> NewsAPI
    :<|> "users" :> UsersAPI

server :: Server AppAPI
server =
  categories
    :<|> images
    :<|> news
    :<|> users

api :: Proxy AppAPI
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
