{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Users where

import API.Modifiers.Beam.Filterable
  ( FilteringApp (FilteringApp),
    FilteringRequestBeam (filterByRequest_),
    filterFor_,
  )
import API.Modifiers.Beam.Sortable
  ( ColumnList (ColNil),
    SortingApp (SortingApp),
    sortBy_,
    sorterFor_,
    (.:.),
  )
import API.Modifiers.Filterable
  ( FilterableBy,
    FilteringRequest,
    Tagged (Tagged),
  )
import API.Modifiers.Paginated (Paginated, Pagination (..))
import API.Modifiers.Protected (AdminUser, Protected)
import API.Modifiers.Sortable
  ( SortableBy,
    Sorting (Ascend),
    SortingRequest (unSortingRequest),
  )
import App.Error
import App.Monad (App)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB (NewsDB (_newsUsers), newsDB)
import Data.Aeson as A
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.ByteString.Lazy (fromStrict)
import Data.CaseInsensitive as CI (CI (original))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Extended as T
import Data.Time
  ( Day (ModifiedJulianDay),
    UTCTime (UTCTime, utctDay, utctDayTime),
  )
import Data.Time.Clock (picosecondsToDiffTime)
import Database.Beam
  ( all_,
    limit_,
    offset_,
    runSelectReturningList,
    select,
  )
import Effects.Config (MonadConfig)
import Effects.Database as DB (MonadDatabase (..))
import Effects.Log as Log (MonadLog, logInfo, logWarning)
import Entities.User
  ( NewUserCredentials (..),
    User,
    UserT
      ( User,
        _userId,
        _userIsAdmin,
        _userIsAllowedToPost,
        _userLogin,
        _userName,
        _userPasswordHash,
        _userPasswordHashIterations,
        _userPasswordSalt,
        _userRegistrationDate
      ),
    insertNewUser,
    lookupUserLogin,
  )
import Servant
  ( Get,
    JSON,
    PostCreated,
    ReqBody,
    ServerError (errBody),
    ServerT,
    err500,
    err503,
    throwError,
    (:<|>) (..),
    (:>),
  )
import Servant.Docs as Docs (ToSample (toSamples), singleSample)

type UsersAPI =
  Paginated
    :> SortableBy
         '[ "name",
            "login",
            "registration-date",
            "is-admin",
            "is-allowed-to-post"
          ]
         ('Ascend "name")
    :> FilterableBy
         '[ 'Tagged "name" T.Text,
            'Tagged "login" (CI T.Text),
            'Tagged "registration-date" UTCTime,
            'Tagged "is-admin" Bool,
            'Tagged "is-allowed-to-post" Bool
          ]
    :> Get '[JSON] [UserJSON]
    :<|> Protected AdminUser :> ReqBody '[JSON] NewUserJSON :> PostCreated '[JSON] UserJSON

users :: ServerT UsersAPI App
users = listUsers :<|> addNewUser

newtype UserJSON = UserJSON User

instance A.ToJSON UserJSON where
  toJSON (UserJSON User {..}) =
    A.object
      [ "name" A..= _userName,
        "login" A..= CI.original _userLogin,
        "registration-date" A..= _userRegistrationDate,
        "is-admin" A..= _userIsAdmin,
        "is-allowed-to-post" A..= _userIsAllowedToPost
      ]

instance Docs.ToSample UserJSON where
  toSamples _ = Docs.singleSample $ UserJSON user
    where
      dateTime =
        UTCTime
          { utctDay = ModifiedJulianDay 123456,
            utctDayTime = picosecondsToDiffTime 123456
          }
      user =
        User
          { _userId = 1234,
            _userName = "Name",
            _userLogin = "Login",
            _userPasswordHash = "passwordHash",
            _userPasswordHashIterations = 1234,
            _userPasswordSalt = "salt",
            _userRegistrationDate = dateTime,
            _userIsAdmin = False,
            _userIsAllowedToPost = True
          }

listUsers ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadConfig m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  Pagination ->
  SortingRequest
    '[ "name",
       "login",
       "registration-date",
       "is-admin",
       "is-allowed-to-post"
     ]
    ('Ascend "name") ->
  FilteringRequest
    '[ 'Tagged "name" T.Text,
       'Tagged "login" (CI T.Text),
       'Tagged "registration-date" UTCTime,
       'Tagged "is-admin" Bool,
       'Tagged "is-allowed-to-post" Bool
     ] ->
  m [UserJSON]
listUsers Pagination {..} sorting fReq = do
  Log.logInfo $ "Get /user sort-by=" <> T.tshow (unSortingRequest sorting)
  usrs <-
    DB.runQuery
      . runSelectReturningList
      . select
      . filterByRequest_ fReq filters
      . limit_ limit
      . offset_ offset
      . sortBy_ sorting sorters
      . all_
      $ _newsUsers newsDB
  pure $ UserJSON <$> usrs
  where
    sorters User {..} =
      SortingApp
        ( sorterFor_ @"name" _userName
            .:. sorterFor_ @"login" _userLogin
            .:. sorterFor_ @"registration-date" _userRegistrationDate
            .:. sorterFor_ @"is-admin" _userIsAdmin
            .:. sorterFor_ @"is-allowed-to-post" _userIsAllowedToPost
            .:. ColNil
        )
    filters User {..} =
      FilteringApp
        ( filterFor_ @"is-admin" _userIsAdmin
            .:. filterFor_ @"name" _userName
            .:. filterFor_ @"login" _userLogin
            .:. filterFor_ @"registration-date" _userRegistrationDate
            .:. filterFor_ @"is-admin" _userIsAdmin
            .:. filterFor_ @"is-allowed-to-post" _userIsAllowedToPost
            .:. ColNil
        )

addNewUser ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m,
    MonadThrow m,
    MonadCatch m
  ) =>
  User ->
  NewUserJSON ->
  m UserJSON
addNewUser creator (NewUserJSON newUser) = do
  flip catch dealWithAPIerror $ insertNewUser table newUser
  doCheckIfSuccessfull
  where
    table = _newsUsers newsDB
    newUserLogin = _newUserLogin newUser
    creatorLogin = CI.original (_userLogin creator)
    dealWithAPIerror e = case e of
      APIError msg -> throwError $ err500 {errBody = fromStrict $ encodeUtf8 msg}
      other -> throwM other
    doCheckIfSuccessfull = do
      newUserMaybe <- lookupUserLogin table newUserLogin
      case newUserMaybe of
        Nothing -> doLogDBError >> throwError err503
        Just u -> doLogSuccess >> return (UserJSON u)
    doLogSuccess =
      Log.logInfo $
        "User \"" <> creatorLogin <> "\" created new user :\"" <> newUserLogin <> "\""
    doLogDBError =
      Log.logWarning $
        "User \"" <> newUserLogin <> "\" was not added to Database"

newtype NewUserJSON = NewUserJSON NewUserCredentials

instance A.FromJSON NewUserJSON where
  parseJSON = A.withObject "NewUserJSON" $ \o -> do
    _newUserLogin <- o .: "login"
    _newUserName <- o .:? "name" .!= _newUserLogin
    _newUserPassword <- o .: "password"
    _newUserIsAdmin <- o .:? "is-admin" .!= False
    _newUserIsAllowedToPost <- o .:? "is-allowed-to-post" .!= False
    return $ NewUserJSON $ NewUser {..}

instance A.ToJSON NewUserJSON where
  toJSON (NewUserJSON (NewUser {..})) =
    A.object
      [ "login" A..= _newUserLogin,
        "name" A..= _newUserName,
        "password" A..= _newUserPassword,
        "is-admin" A..= _newUserIsAdmin,
        "is-allowed-to-post" A..= _newUserIsAllowedToPost
      ]

instance Docs.ToSample NewUserJSON where
  toSamples _ = Docs.singleSample $ NewUserJSON newUser
    where
      newUser =
        NewUser
          { _newUserName = "name",
            _newUserLogin = "login",
            _newUserPassword = "password",
            _newUserIsAdmin = False,
            _newUserIsAllowedToPost = True
          }
