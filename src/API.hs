{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.Categories
import API.Images
import API.News
import API.Users
import App.Env
import App.Monad
import DB
import Data.CaseInsensitive as CI (mk)
import Data.Text.Encoding (decodeUtf8)
import Database.Beam
import Entities.User
import qualified Handlers.Database as DB
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

appServer :: Context '[BasicAuthCheck User] -> AppEnv -> Server AppAPI
appServer _ctx env =
  hoistServerWithContext
    api
    (Proxy :: Proxy '[BasicAuthCheck User])
    (appToHandler env)
    server

app :: AppEnv -> Application
app env = serveWithContext api ctx $ appServer ctx env
  where
    ctx = checkBasicAuth (envDatabase env) :. EmptyContext

checkBasicAuth :: DB.Handle -> BasicAuthCheck User
checkBasicAuth dbHandle = BasicAuthCheck $ \basicAuthData -> do
  let username = CI.mk $ decodeUtf8 (basicAuthUsername basicAuthData)
  let password = basicAuthPassword basicAuthData
  maybeUser <-
    DB.runDBQuery dbHandle
      . runSelectReturningOne
      . select
      . filter_ (\u -> _userLogin u ==. val_ username)
      $ all_ (_newsUsers newsDB)
  case maybeUser of
    Nothing -> pure NoSuchUser
    Just user -> if checkPassword password user then pure (Authorized user) else pure BadPassword
