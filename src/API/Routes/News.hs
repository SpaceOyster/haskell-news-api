{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.News where

import App.Monad
import Data.Text
import Data.Aeson as A
import Data.Int
import Data.Text.Extended as T
import Data.Time.Clock
import Entities.Image
import Servant

type NewsAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

news :: ServerT NewsAPI App
news = return "GET categories endpoint" :<|> return "POST categories endpoint"

data ArticleJSON = ArticleJSON
  { _articleJSONId :: Int32,
    _articleJSONTitle :: Text,
    _articleJSONCreatedAt :: UTCTime,
    _articleJSONAuthorName :: Text,
    _articleJSONCategory :: Maybe Text,
    _articleJSONBody :: Text,
    _articleJSONImages :: [FileNameJSON],
    _articleJSONIsPublished :: Bool
  }
  deriving (Show)


instance A.ToJSON ArticleJSON where
  toJSON (ArticleJSON {..}) =
    A.object
      [ "id" A..= _articleJSONId,
        "title" A..= _articleJSONTitle,
        "created-at" A..= _articleJSONCreatedAt,
        "author" A..= _articleJSONAuthorName,
        "category" A..= _articleJSONCategory,
        "body" A..= _articleJSONBody,
        "images" A..= _articleJSONImages,
        "is-published" A..= _articleJSONIsPublished
      ]
newtype FileNameJSON = FileNameJSON {unFileNameJSON :: FileName}
  deriving (Show)

instance A.ToJSON FileNameJSON where
  toJSON (FileNameJSON fn) = A.String $ "/images/" <> T.tshow fn

instance A.FromJSON FileNameJSON where
  parseJSON = A.withText "FileNameJSON" (fmap FileNameJSON . parseFileName)
