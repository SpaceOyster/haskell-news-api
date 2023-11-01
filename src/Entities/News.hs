{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.News where

import Data.Int
import Data.Text
import Data.Time.Clock
import Database.Beam
import Database.Beam.Postgres
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

data ArticleImageT f = ArticleImage
  { _articleImageArticleId :: PrimaryKey ArticleT f,
    _articleImageImageId :: PrimaryKey ImageT f
  }
  deriving (Generic, Beamable)

type ArticleImage = ArticleImageT Identity

deriving instance Show ArticleImage

deriving instance Eq ArticleImage

instance Table ArticleImageT where
  data PrimaryKey ArticleImageT f = ArticleImageId (PrimaryKey ArticleT f) (PrimaryKey ImageT f) deriving (Generic, Beamable)
  primaryKey = ArticleImageId <$> _articleImageArticleId <*> _articleImageImageId

type ArticleImageId = PrimaryKey ArticleImageT Identity

deriving instance Show (PrimaryKey ArticleImageT Identity)

deriving instance Eq (PrimaryKey ArticleImageT Identity)

articleImageReletaionship ::
  (Database be db) =>
  DatabaseEntity be db (TableEntity ArticleImageT) ->
  ManyToMany be db ArticleT ImageT
articleImageReletaionship articleImageT =
  manyToMany_ articleImageT _articleImageArticleId _articleImageImageId

selectArticleWithAuthor ::
  (MonadBeam Postgres m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity ArticleT) ->
  DatabaseEntity Postgres db (TableEntity UserT) ->
  ArticleId ->
  m (Maybe (Article, User))
selectArticleWithAuthor articleT usersT articleId =
  runSelectReturningOne . select $ do
    article <- filter_ (\a -> pk a ==. val_ articleId) (all_ articleT)
    user <- related_ usersT (_articleAuthor article)
    pure (article, user)

selectArticleImages ::
  (MonadBeam Postgres m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity ArticleT) ->
  DatabaseEntity Postgres db (TableEntity ImageT) ->
  DatabaseEntity Postgres db (TableEntity ArticleImageT) ->
  Int32 ->
  m [Image]
selectArticleImages articleT imageT articleImageT articleId =
  fmap (fmap snd)
    . runSelectReturningList
    . select
    $ articleImageReletaionship
      articleImageT
      (filter_ (\a -> _articleId a ==. val_ articleId) (all_ articleT))
      (all_ imageT)
