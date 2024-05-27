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
import Data.Int (Int32)
import qualified Data.List as L (delete, find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Extended as T
import Database.Beam
import Database.Beam.Postgres
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
    :<|> Protected AdminUser :> ReqBody '[JSON] NewCategoryJSON :> PostCreated '[JSON] CategoryJSON


data CategoryJSON = CategoryJSON
  { _categoryJSONName :: CI T.Text,
    _categoryJSONParent :: Maybe CategoryJSON
  }
  deriving (Show)

instance A.ToJSON CategoryJSON where
  toJSON (CategoryJSON {..}) = A.object $ ["name" A..= CI.original _categoryJSONName] <> maybeParentField
    where
      maybeParentField = maybe [] (\i -> ["parent" A..= i]) _categoryJSONParent

instance A.FromJSON CategoryJSON where
  parseJSON = A.withObject "CategoryJSON" $ \o -> do
    _categoryJSONName <- CI.mk <$> o A..: "name"
    _categoryJSONParent <- o A..: "parent"
    return $ CategoryJSON {..}

instance Docs.ToSample CategoryJSON where
  toSamples _ =
    [ ("Category may have no parent", cat1),
      ("Category may have a parent", cat2)
    ]
    where
      cat1 =
        CategoryJSON
          { _categoryJSONName = "Outer Space",
            _categoryJSONParent = Nothing
          }
      parentCat =
        CategoryJSON
          { _categoryJSONName = "Functional Language",
            _categoryJSONParent = Nothing
          }
      cat2 =
        CategoryJSON
          { _categoryJSONName = "Haskell",
            _categoryJSONParent = Just parentCat
          }

newtype NewCategoryJSON = NewCategoryJSON {getNewCategory :: NewCategory}

instance A.ToJSON NewCategoryJSON where
  toJSON (NewCategoryJSON (NewCategory {..})) = A.object $ ["name" A..= _newCategoryName] <> maybeParentField
    where
      maybeParentField = maybe [] (\i -> ["parent" A..= i]) _newCategoryParent

instance A.FromJSON NewCategoryJSON where
  parseJSON = A.withObject "NewCategoryJSON" $ \o -> do
    _newCategoryName <- o A..: "name"
    _newCategoryParent <- o A..: "parent"
    return $ NewCategoryJSON $ NewCategory {..}

instance Docs.ToSample NewCategoryJSON where
  toSamples _ =
    [ ("Category may have no parent", cat1),
      ("Category may have a parent", cat2)
    ]
    where
      cat1 =
        NewCategoryJSON $
          NewCategory
            { _newCategoryName = "Outer Space",
              _newCategoryParent = Nothing
            }
      cat2 =
        NewCategoryJSON $
          NewCategory
            { _newCategoryName = "Haskell",
              _newCategoryParent = Just "Functional Language"
            }

data CategoryUpdateJSON = CategoryUpdateJSON
  { _categoryUpdateJSONName :: Maybe (CI T.Text),
    _categoryUpdateJSONParent :: Maybe (CI T.Text)
  }
  deriving (Show)

instance A.FromJSON CategoryUpdateJSON where
  parseJSON = A.withObject "CategoryUpdateJSON" $ \o -> do
    _categoryUpdateJSONName <- fmap CI.mk <$> o A..:? "name"
    _categoryUpdateJSONParent <- fmap CI.mk <$> o A..:? "parent"
    pure CategoryUpdateJSON {..}

categories :: ServerT CategoriesAPI App
categories = listCategories :<|> postCategory

listCategories ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  Pagination ->
  SortingRequest '["name", "parent"] ('Ascend "name") ->
  FilteringRequest
    '[ 'Tagged "name" (CI T.Text), 'Tagged "parent" (CI T.Text)] ->
  m [CategoryJSON]
listCategories (Pagination {..}) sorting fReq = do
  Log.logInfo $ "Get /categories sort-by=" <> T.tshow (unSortingRequest sorting)
  catPairs <-
    DB.runQuery
      . runSelectReturningList
      . select
      . limit_ limit
      . offset_ offset
      . filterByRequest_ fReq filters
      . sortBy_ sorting sorters
      $ do
        category <- all_ $ _newsCategories newsDB
        parentM <- leftJoin_ (all_ $ _newsCategories newsDB) (\p -> maybe_ (val_ False) (`references_` p) (_categoryParentCategory category))
        pure (category, parentM)
  mapM fetchParentsMkJSON catPairs
  where
    sorters (c, pM) =
      SortingApp
        ( sorterFor_ @"name" (_categoryName c)
            .:. sorterFor_ @"parent" (_categoryName pM)
            .:. ColNil
        )
    filters (c, pM) =
      FilteringApp
        ( filterFor_ @"name" (_categoryName c)
            .:. filterFor_ @"parent" (fromMaybe_ (val_ "") $ _categoryName pM) --  | treat empty columns as empty strings
            .:. ColNil
        )
    fetchParentsMkJSON (c, pM) = do
      pJSONMaybe <- maybe (pure Nothing) (categoryWithParentsById (_newsCategories newsDB) . _categoryId) pM
      pure $ CategoryJSON (_categoryName c) pJSONMaybe

postCategory ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m,
    MonadThrow m,
    MonadCatch m
  ) =>
  User ->
  NewCategoryJSON ->
  m CategoryJSON
postCategory usr (NewCategoryJSON cat) = do
  flip catch dealWithAPIerror $ insertNewCategory table cat
  doCheckIfSuccessfull
  where
    table = _newsCategories newsDB
    creatorLogin = CI.original (_userLogin usr)
    dealWithAPIerror e = case e of
      APIError msg -> throwError $ err500 {errBody = fromStrict $ T.encodeUtf8 msg}
      other -> throwM other
    doCheckIfSuccessfull = do
      newCatMaybe <- categoryWithParents table $ CI.mk (_newCategoryName cat)
      case newCatMaybe of
        Nothing -> doLogDBError >> throwError err503
        Just c -> doLogSuccess >> return c
    doLogSuccess =
      Log.logInfo $
        "User \"" <> creatorLogin <> "\" created new category :\"" <> T.tshow cat <> "\""
    doLogDBError =
      Log.logWarning $
        "Category \"" <> T.tshow cat <> "\" was not added to Database"


categoryWithParents ::
  (MonadDatabase m, MonadIO m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI T.Text ->
  m (Maybe CategoryJSON)
categoryWithParents table name =
  fmap (toCategoryJSON name) . DB.runQuery $ lookupCategoryWithAncestors table name

categoryWithParentsById ::
  (MonadDatabase m, MonadIO m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  Int32 ->
  m (Maybe CategoryJSON)
categoryWithParentsById table cId =
  fmap (toCategoryJSONById cId) . DB.runQuery $ lookupCategoryIdWithAncestors table cId

toCategoryJSON :: CI T.Text -> [Category] -> Maybe CategoryJSON
toCategoryJSON name xs = do
  cat <- L.find ((== name) . _categoryName) xs
  let withoutCat = L.delete cat xs
      parentIdM = unCategoryId $ _categoryParentCategory cat
      rest = parentIdM >>= (`toCategoryJSONById` withoutCat)
  pure $ CategoryJSON (_categoryName cat) rest

toCategoryJSONById :: Int32 -> [Category] -> Maybe CategoryJSON
toCategoryJSONById cId xs = do
  cat <- L.find ((== cId) . _categoryId) xs
  let withoutCat = L.delete cat xs
      parentIdM = unCategoryId $ _categoryParentCategory cat
      rest = parentIdM >>= (`toCategoryJSONById` withoutCat)
  pure $ CategoryJSON (_categoryName cat) rest
