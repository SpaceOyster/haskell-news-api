{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.Category where

import Data.CaseInsensitive (CI)
import Data.Int
import Data.Maybe (isJust)
import Data.Text
import Database.Beam
import Database.Beam.Postgres

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

categoryExists ::
  (MonadBeam Postgres m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI Text ->
  m Bool
categoryExists table catname = isJust <$> lookupCategory table catname

lookupCategory ::
  (Database Postgres db, MonadBeam Postgres m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI Text ->
  m (Maybe Category)
lookupCategory table catName =
  runSelectReturningOne
    . select
    . filter_ (\c -> _categoryName c ==. val_ catName)
    $ all_ table

lookupCategoryId ::
  (Database Postgres db, MonadBeam Postgres m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  Int32 ->
  m (Maybe Category)
lookupCategoryId table cId =
  runSelectReturningOne $ lookup_ table (CategoryId cId)

lookupCategoryWithAncestors ::
  (Database Postgres db, MonadBeam Postgres m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI Text ->
  m [Category]
lookupCategoryWithAncestors table catName =
  runSelectReturningList . selectWith $ do
    rec c <-
          selecting $
            union_
              (filter_ (\x -> _categoryName x ==. val_ catName) (all_ table))
              (reuse c >>= \c' -> filter_ (\x -> _categoryParentCategory c' ==. just_ (pk x)) (all_ table))
    pure (reuse c)

lookupCategoryIdWithAncestors ::
  (Database Postgres db, MonadBeam Postgres m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  Int32 ->
  m [Category]
lookupCategoryIdWithAncestors table cId =
  runSelectReturningList . selectWith $ do
    rec c <-
          selecting $
            union_
              (filter_ (\x -> pk x ==. CategoryId (val_ cId)) (all_ table))
              (reuse c >>= \c' -> filter_ (\x -> _categoryParentCategory c' ==. just_ (pk x)) (all_ table))
    pure (reuse c)

lookupCategoryWithDescendants ::
  (Database Postgres db, MonadBeam Postgres m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI Text ->
  m [Category]
lookupCategoryWithDescendants table catName =
  runSelectReturningList . selectWith $ do
    rec c <-
          selecting $
            union_
              (filter_ (\x -> _categoryName x ==. val_ catName) (all_ table))
              (reuse c >>= \c' -> filter_ (\x -> _categoryParentCategory x ==. just_ (pk c')) (all_ table))
    pure (reuse c)

lookupCategoryIdWithDescendants ::
  (Database Postgres db, MonadBeam Postgres m) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  Int32 ->
  m [Category]
lookupCategoryIdWithDescendants table cId =
  runSelectReturningList . selectWith $ do
    rec c <-
          selecting $
            union_
              (filter_ (\x -> pk x ==. CategoryId (val_ cId)) (all_ table))
              (reuse c >>= \c' -> filter_ (\x -> _categoryParentCategory x ==. just_ (pk c')) (all_ table))
    pure (reuse c)
