{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.News where

import App.Monad
import Control.Monad.Catch (MonadCatch (catch), MonadThrow, throwM)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.Bifunctor (second)
import Data.CaseInsensitive as CI
import Data.Int
import Data.Text.Extended as T
import Data.Time.Clock
import Database.Beam
import Database.Beam.Postgres
import Effects.Database as DB
import Effects.Log as Log
import Entities.Category
import Entities.Image
import Entities.News
import Entities.User
import Servant

type NewsAPI =
  Capture "id" Int32 :> Get '[JSON] ArticleJSON
    :<|> Protected AuthorUser :> ReqBody '[JSON] ArticleJSON :> PostCreated '[JSON] ArticleJSON

news :: ServerT NewsAPI App
news = getArticle :<|> postArticle

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

getArticle ::
  ( MonadDatabase m,
    MonadIO m,
    MonadThrow m,
    MonadError ServerError m,
    MonadLog m
  ) =>
  Int32 ->
  m ArticleJSON
getArticle articleId = do
  Log.logInfo $ "Article " <> T.tshow articleId <> " is requested"
  (Article {..}, authorName) <- lookupArticleByIdWithAuthorName
  imgs <- DB.runQuery selectArticleImageFileNames
  pure $
    ArticleJSON
      { _articleJSONId = _articleId,
        _articleJSONTitle = _articleTitle,
        _articleJSONCreatedAt = _articleCreatedAt,
        _articleJSONAuthorName = authorName,
        _articleJSONCategory = CI.original <$> unCategoryId _articleCategory,
        _articleJSONBody = _articleBody,
        _articleJSONImages = FileNameJSON <$> imgs,
        _articleJSONIsPublished = _articleIsPublished
      }
  where
    articleT = _newsArticles newsDB
    imageT = _newsImages newsDB
    articleImageT = _newsArticlesImages newsDB
    usersT = _newsUsers newsDB
    lookupArticleByIdWithAuthorName = do
      let onlyName = fmap (fmap (second _userName))
      xMaybe <- DB.runQuery . onlyName . selectArticleWithAuthor articleT usersT $ ArticleId articleId
      case xMaybe of
        Nothing -> doLogNotFound >> throwError err404
        Just x -> pure x
    selectArticleImageFileNames =
      fmap _imageIdFileName <$> selectArticleImages articleT imageT articleImageT articleId
    doLogNotFound = Log.logInfo $ "Article " <> T.tshow articleId <> " not found"

postArticle = undefined
insertArticle ::
  (MonadBeam Postgres m, MonadBeam Postgres m) =>
  User ->
  ArticlePostJSON ->
  m (Maybe (ArticleT Identity))
insertArticle creator (ArticlePostJSON {..}) = do
  runInsert . insert (_newsArticles newsDB) $
    insertExpressions
      [ Article
          { _articleId = default_,
            _articleTitle = val_ _articlePostJSONTitle,
            _articleCreatedAt = default_,
            _articleAuthor = UserId (val_ $ _userId creator),
            _articleCategory = CategoryId $ val_ (CI.mk <$> _articlePostJSONCategory),
            _articleBody = val_ _articlePostJSONBody,
            _articleIsPublished = val_ _articlePostJSONIsPublished
          }
      ]
  article <-
    runSelectReturningOne
      . select
      . filter_
        ( \a ->
            _articleTitle a
              ==. val_ _articlePostJSONTitle
              &&. _articleAuthor a
              ==. UserId (val_ $ _userId creator)
        )
      $ all_ (_newsArticles newsDB)
  mapM_ (insertArticleImagesRelations imgNames) article
  pure article
  where
    imgNames = unFileNameJSON <$> _articlePostJSONImages

insertArticleImagesRelations ::
  (MonadBeam Postgres m) =>
  [FileName] ->
  Article ->
  m ()
insertArticleImagesRelations imgNames article =
  runInsert . insert (_newsArticlesImages newsDB) . insertFrom $ do
    img <-
      filter_
        (\i -> imgNameString i `in_` imgIdFileNames)
        (all_ (_newsImages newsDB))
    pure (ArticleImage (artPk article) (pk img))
  where
    imgIdFileNames = val_ . T.tshow <$> imgNames
    imgNameString i = concat_ [cast_ (_imageId i) (varchar Nothing), val_ ".", _imageFileExtension i]
    artPk art = ArticleId $ val_ $ _articleId art
