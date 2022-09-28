{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.Modifiers.Protected
import API.Routes.Categories
import API.Routes.Images
import API.Routes.News
import API.Routes.Users
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

contextType :: Proxy '[AuthHandler Request User]
contextType = Proxy

appServer :: AppEnv -> Server AppAPI
appServer env =
  hoistServerWithContext api contextType (appToHandler env) server

app :: AppEnv -> Application
app env = serveWithContext api ctx $ appServer env
  where
    ctx :: Context '[AuthHandler Request User]
    ctx = authHandler env :. EmptyContext
