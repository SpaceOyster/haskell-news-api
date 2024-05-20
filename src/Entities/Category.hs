{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.Category where

import App.Error (apiError)
import Control.Monad (forM_, unless, when)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Int
import Data.Maybe (isJust)
import Data.Text
import Database.Beam
import Database.Beam.Postgres
import Effects.Database

data CategoryT f = Category
  { _categoryId :: Columnar f Int32,
    _categoryName :: Columnar f (CI Text),
    _categoryParentCategory :: PrimaryKey CategoryT (Nullable f)
  }
  deriving (Generic, Beamable)

type Category = CategoryT Identity

deriving instance Show Category

deriving instance Eq Category

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId {unCategoryId :: Columnar f Int32}
    deriving (Generic, Beamable)
  primaryKey = CategoryId . _categoryId

type CategoryId = PrimaryKey CategoryT Identity


deriving instance Show (PrimaryKey CategoryT Identity)

deriving instance Show (PrimaryKey CategoryT (Nullable Identity))

deriving instance Eq (PrimaryKey CategoryT Identity)

deriving instance Eq (PrimaryKey CategoryT (Nullable Identity))

data NewCategory = NewCategory
  { _newCategoryName :: Text,
    _newCategoryParent :: Maybe Text
  }
  deriving (Show)

insertNewCategory ::
  (MonadDatabase m, MonadIO m, Database Postgres db, MonadThrow m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  Category ->
  m ()
insertNewCategory table newcat = do
  checkIfCategoryExists
  let maybeParent = _categoryParentName newcat
  forM_ maybeParent checkIfParentExists
  runQuery . runInsert . insert table . insertValues . (: []) $ newcat
  where
    newCatName = _categoryName newcat
    checkIfCategoryExists = do
      yes <- categoryExists table newCatName
      let msg = "Category \"" <> CI.original newCatName <> "\" already exists"
      when yes (throwM $ apiError msg)
    checkIfParentExists parent = do
      yes <- categoryExists table parent
      let msg = "Parent Category \"" <> CI.original parent <> "\" doesn't exist"
      unless yes (throwM $ apiError msg)

categoryExists ::
  (MonadDatabase m, MonadIO m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI Text ->
  m Bool
categoryExists table catname = isJust <$> runQuery (lookupCategory table catname)

lookupCategory ::
  (MonadIO m, Database Postgres db, MonadBeam Postgres m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI Text ->
  m (Maybe Category)
lookupCategory table catName =
  runSelectReturningOne
    . select
    . filter_ (\c -> _categoryName c ==. val_ catName)
    $ all_ table
