{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.News where

import Data.Int
import Data.Text
import Data.Time.Clock
import Database.Beam
import Entities.Category
import Entities.Image
import Entities.User

data ArticleT f = Article
  { _articleId :: Columnar f Int32,
    _articleTitle :: Columnar f Text,
    _articleCreatedAt :: Columnar f UTCTime,
    _articleAuthor :: PrimaryKey UserT f,
    _articleCategory :: PrimaryKey CategoryT (Nullable f),
    _articleBody :: Columnar f Text,
    -- _articleImages :: PrimaryKey ImageT f,
    _articleIsPublished :: Columnar f Bool
  }
  deriving (Generic, Beamable)

type Article = ArticleT Identity

deriving instance Show Article

deriving instance Eq Article

instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = ArticleId . _articleId

type ArticleId = PrimaryKey ArticleT Identity

deriving instance Show (PrimaryKey ArticleT Identity)

deriving instance Eq (PrimaryKey ArticleT Identity)