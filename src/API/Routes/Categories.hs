{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Categories where

import App.Monad
import Data.Aeson as A
import Data.CaseInsensitive as CI (CI (original), mk)
import Data.Text
import Entities.Category
import Servant
import Servant.Docs as Docs (ToSample (toSamples), singleSample)

type CategoriesAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

newtype CategoryJSON = CategoryJSON Category

instance A.ToJSON CategoryJSON where
  toJSON (CategoryJSON Category {..}) =
    A.object $ ["name" A..= CI.original _categoryName] <> maybeParentField
    where
      catIdM = case _categoryParentCategory of
        CategoryId idMaybe -> idMaybe
      maybeParentField = maybe [] (\i -> ["parent" A..= CI.original i]) catIdM

instance A.FromJSON CategoryJSON where
  parseJSON = A.withObject "CategoryJSON" $ \o -> do
    _categoryName <- CI.mk <$> o A..: "name"
    _categoryParentCategory <- CategoryId . fmap CI.mk <$> o A..:? "parent"
    return $ CategoryJSON $ Category {..}

instance Docs.ToSample CategoryJSON where
  toSamples _ =
    [ ("Category may have no parent", CategoryJSON cat1),
      ("Category can have a parent", CategoryJSON cat2)
    ]
    where
      cat2 =
        Category
          { _categoryName = "Haskell",
            _categoryParentCategory = CategoryId $ Just "Functional Programing"
          }
      cat1 =
        Category
          { _categoryName = "Outer Space",
            _categoryParentCategory = CategoryId Nothing
          }

categories :: ServerT CategoriesAPI App
categories = return "GET categories endpoint" :<|> return "POST categories endpoint"
