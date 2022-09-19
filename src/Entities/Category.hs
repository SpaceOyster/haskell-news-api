{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.Category where

import Data.CaseInsensitive as CI
import Data.Text
import Database.Beam

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
