{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module DB where

import API.Entities.Category
import API.Entities.Image
import API.Entities.News
import API.Entities.User
import Database.Beam

data NewsDB f = NewsDB
  { _newsImages :: f (TableEntity ImageT),
    _newsUsers :: f (TableEntity UserT),
    _newsCategories :: f (TableEntity CategoryT),
    _newsArticles :: f (TableEntity ArticleT)
  }
  deriving (Generic, Database be)

newsDB :: DatabaseSettings be NewsDB
newsDB = defaultDbSettings
