{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module DB where

import Database.Beam
import Entities.Category
import Entities.Image
import Entities.News
import Entities.User

data NewsDB f = NewsDB
  { _newsImages :: f (TableEntity ImageT),
    _newsUsers :: f (TableEntity UserT),
    _newsCategories :: f (TableEntity CategoryT),
    _newsArticles :: f (TableEntity ArticleT)
  }
  deriving (Generic, Database be)

newsDB :: DatabaseSettings be NewsDB
newsDB = defaultDbSettings
