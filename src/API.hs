{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.Categories
import API.Images
import API.News
import API.Protected
import API.Users
import App.Monad
import Entities.User
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth

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

basicAuthProxy :: Proxy '[AuthHandler Request User]
basicAuthProxy = Proxy

appServer :: Context '[AuthHandler Request User] -> AppEnv -> Server AppAPI
appServer _ctx env =
  hoistServerWithContext api basicAuthProxy (appToHandler env) server

app :: AppEnv -> Application
app env = serveWithContext api ctx $ appServer ctx env
  where
    ctx :: Context '[AuthHandler Request User]
    ctx = authHandler env :. EmptyContext
