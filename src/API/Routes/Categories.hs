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

type CategoriesAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

newtype CategoryJSON = CategoryJSON Category

instance A.ToJSON CategoryJSON where
  toJSON (CategoryJSON Category {..}) =
    A.object $ ["name" A..= CI.original _categoryName] <> maybeParentField
    where
      catIdM = case _categoryParentCategory of
        CategoryId idMaybe -> idMaybe
      maybeParentField = maybe [] (\i -> ["parent" A..= CI.original i]) catIdM

categories :: ServerT CategoriesAPI App
categories = return "GET categories endpoint" :<|> return "POST categories endpoint"
