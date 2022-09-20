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
import Servant.Server.Experimental.Auth
import Servant.Server.Internal.BasicAuth

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

lookupAccount :: AppEnv -> BasicAuthData -> Handler User
lookupAccount env basicAuthData = do
  let username = CI.mk $ decodeUtf8 (basicAuthUsername basicAuthData)
  let password = basicAuthPassword basicAuthData
  maybeUser <-
    liftIO
      . DB.runDBQuery (envDatabase env)
      . runSelectReturningOne
      . select
      . filter_ (\u -> _userLogin u ==. val_ username)
      $ all_ (_newsUsers newsDB)
  case maybeUser of
    Nothing -> throwError err404
    Just user -> if checkPassword password user then pure user else throwError err404

authHandler :: AppEnv -> AuthHandler Request User
authHandler env = mkAuthHandler handler
  where
    handler req = do
      let maybeBasicAuthData = decodeBAHdr req
      maybe (throwError err404) (lookupAccount env) maybeBasicAuthData
