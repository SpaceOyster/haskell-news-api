{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Categories where

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
import App.Monad
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.ByteString.Lazy (fromStrict)
import Data.CaseInsensitive as CI (CI (original), mk)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Extended as T
import Database.Beam
import Effects.Database as DB (MonadDatabase (..))
import Effects.Log as Log (MonadLog, logInfo, logWarning)
import Entities.Category
import Entities.User
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
import Servant.Docs as Docs (ToSample (toSamples))

type CategoriesAPI =
  Paginated
    :> SortableBy '["name", "parent"] ('Ascend "name")
    :> FilterableBy
         '[ 'Tagged "name" (CI T.Text),
            'Tagged "parent" (CI T.Text)
          ]
    :> Get '[JSON] [CategoryJSON]
    :<|> Protected AdminUser :> ReqBody '[JSON] CategoryJSON :> PostCreated '[JSON] CategoryJSON

newtype CategoryJSON = CategoryJSON Category

instance A.ToJSON CategoryJSON where
  toJSON (CategoryJSON Category {..}) =
    A.object $ ["name" A..= CI.original _categoryName] <> maybeParentField
    where
      catIdM = case _categoryParentCategory of
        CategoryId idMaybe -> idMaybe
      maybeParentField = maybe [] (\i -> ["parent" A..= CI.original i]) catIdM

instance A.FromJSON CategoryJSON where
  parseJSON = A.withObject "CategoryJSON" $ \o -> do
    _categoryName <- CI.mk <$> o A..: "name"
    _categoryParentCategory <- CategoryId . fmap CI.mk <$> o A..:? "parent"
    return $ CategoryJSON $ Category {..}

instance Docs.ToSample CategoryJSON where
  toSamples _ =
    [ ("Category may have no parent", CategoryJSON cat1),
      ("Category can have a parent", CategoryJSON cat2)
    ]
    where
      cat2 =
        Category
          { _categoryName = "Haskell",
            _categoryParentCategory = CategoryId $ Just "Functional Programing"
          }
      cat1 =
        Category
          { _categoryName = "Outer Space",
            _categoryParentCategory = CategoryId Nothing
          }

categories :: ServerT CategoriesAPI App
categories = listCategories :<|> postCategories

listCategories ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  Pagination ->
  SortingRequest '["name", "parent"] ('Ascend "name") ->
  FilteringRequest
    '[ 'Tagged "name" (CI Text), 'Tagged "parent" (CI Text)] ->
  m [CategoryJSON]
listCategories (Pagination {..}) sorting fReq = do
  Log.logInfo $ "Get /categories sort-by=" <> T.tshow (unSortingRequest sorting)
  usrs <-
    DB.runQuery
      . runSelectReturningList
      . select
      . filterByRequest_ fReq filters
      . limit_ limit
      . offset_ offset
      . sortBy_ sorting sorters
      . all_
      $ _newsCategories newsDB
  pure $ CategoryJSON <$> usrs
  where
    sorters Category {..} =
      SortingApp
        ( sorterFor_ @"name" _categoryName
            .:. sorterFor_ @"parent" _categoryName
            .:. ColNil
        )
    filters Category {..} =
      FilteringApp
        ( filterFor_ @"name" _categoryName
            .:. filterFor_ @"parent" _categoryName
            .:. ColNil
        )

postCategories ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m,
    MonadThrow m,
    MonadCatch m
  ) =>
  User ->
  CategoryJSON ->
  m CategoryJSON
postCategories usr (CategoryJSON cat) =
  doCreateCategory
  where
    table = _newsCategories newsDB
    creatorLogin = CI.original (_userLogin usr)
    dealWithAPIerror e = case e of
      APIError msg -> throwError $ err500 {errBody = fromStrict $ encodeUtf8 msg}
      other -> throwM other
    doCreateCategory = do
      flip catch dealWithAPIerror $ insertNewCategory table cat
      doCheckIfSuccessfull
    doCheckIfSuccessfull = do
      newCatMaybe <- lookupCategory table $ _categoryName cat
      case newCatMaybe of
        Nothing -> doLogDBError >> throwError err503
        Just c -> doLogSuccess >> return (CategoryJSON c)
    doLogSuccess =
      Log.logInfo $
        "User \"" <> creatorLogin <> "\" created new category :\"" <> T.tshow cat <> "\""
    doLogDBError =
      Log.logWarning $
        "Category \"" <> T.tshow cat <> "\" was not added to Database"
