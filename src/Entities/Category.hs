{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.Category where

import Data.CaseInsensitive as CI
import Data.Text
import Database.Beam
import Database.Beam.Postgres
import Effects.Database

data CategoryT f = Category
  { _categoryName :: Columnar f (CI Text),
    _categoryParentCategory :: PrimaryKey CategoryT (Nullable f)
  }
  deriving (Generic, Beamable)

type Category = CategoryT Identity

deriving instance Show Category

deriving instance Eq Category

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (Columnar f (CI Text))
    deriving (Generic, Beamable)
  primaryKey = CategoryId . _categoryName

type CategoryId = PrimaryKey CategoryT Identity

deriving instance Show (PrimaryKey CategoryT Identity)

deriving instance Show (PrimaryKey CategoryT (Nullable Identity))

deriving instance Eq (PrimaryKey CategoryT Identity)

deriving instance Eq (PrimaryKey CategoryT (Nullable Identity))

insertNewCategory ::
  (MonadDatabase m, MonadIO m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  Category ->
  m ()
insertNewCategory table cat = do
  runQuery . runInsert . insert table $
    insertExpressions
      [ Category
          { _categoryName = val_ $ _categoryName cat,
            _categoryParentCategory = val_ $ _categoryParentCategory cat
          }
      ]

lookupCategory ::
  (MonadDatabase m, MonadIO m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity CategoryT) ->
  CI Text ->
  m (Maybe Category)
lookupCategory table catName =
  runQuery
    . runSelectReturningOne
    . select
    . filter_ (\c -> _categoryName c ==. val_ catName)
    $ all_ table
