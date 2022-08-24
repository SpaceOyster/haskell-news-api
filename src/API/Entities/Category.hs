{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module API.Entities.Category where

import Data.Text
import Database.Beam

data CategoryT f = Category
  { _categoryName :: Columnar f Text,
    _categoryParentCategory :: Columnar f (Maybe Text)
  }
  deriving (Generic, Beamable)

type Category = CategoryT Identity

deriving instance Show Category

deriving instance Eq Category

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = CategoryId . _categoryName

type CategoryId = PrimaryKey CategoryT Identity

deriving instance Show (PrimaryKey CategoryT Identity)

deriving instance Eq (PrimaryKey CategoryT Identity)
