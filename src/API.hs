{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.Categories
import API.Images
import API.News
import API.Users
import App.Monad
import Network.Wai
import Servant

type AppAPI =
  "categories" :> CategoriesAPI
    :<|> "images" :> ImagesAPI
    :<|> "news" :> NewsAPI
    :<|> "users" :> UsersAPI

server :: ServerT AppAPI App
server =
  categories
    :<|> images
    :<|> news
    :<|> users

api :: Proxy AppAPI
api = Proxy

appServer :: AppEnv -> Server AppAPI
appServer env = hoistServer api (appToHandler env) server

app :: AppEnv -> Application
app = serve api . appServer
