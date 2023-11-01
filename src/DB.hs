{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
    _newsArticles :: f (TableEntity ArticleT),
    _newsArticlesImages :: f (TableEntity ArticleImageT)
  }
  deriving (Generic, Database be)

newsDB :: DatabaseSettings be NewsDB
newsDB =
  defaultDbSettings
    `withDbModification` dbModification
      { _newsCategories =
          modifyTableFields
            tableModification
              { _categoryParentCategory =
                  CategoryId "parent_category"
              },
        _newsArticles =
          modifyTableFields
            tableModification
              { _articleAuthor = UserId "author",
                _articleCategory = CategoryId "category"
              },
        _newsArticlesImages =
          modifyTableFields
            tableModification
              { _articleImageArticleId = ArticleId "article_id",
                _articleImageImageId = ImageId "image_id"
              }
      }
