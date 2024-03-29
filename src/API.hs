{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.Modifiers.Paginated
import API.Modifiers.Protected
import API.Routes.Categories
import API.Routes.Images
import API.Routes.News
import API.Routes.Users
import App.Config
import App.Env
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

contextType :: Proxy (AvailableAuthHandlers .++ '[PaginationConfig])
contextType = Proxy

appServer :: AppEnv -> Server AppAPI
appServer env =
  hoistServerWithContext api contextType (appToHandler env) server

app :: AppEnv -> Application
app env = serveWithContext api ctx $ appServer env
  where
    paginationConfig = pagination $ apiConfig (envConfig env)
    ctx :: Context (AvailableAuthHandlers .++ '[PaginationConfig])
    ctx = authContext env .++ (paginationConfig :. EmptyContext)
