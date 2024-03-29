{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.News where

import API.Modifiers.Beam.Filterable
import API.Modifiers.Beam.Sortable
import API.Modifiers.Filterable
import API.Modifiers.Paginated
import API.Modifiers.Protected
import API.Modifiers.Sortable
import App.Error (AppError (APIError))
import App.Monad
import Control.Monad (forM_)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow, throwM)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.Bifunctor (second)
import Data.CaseInsensitive as CI
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Extended as T
import Data.Time.Clock
import Database.Beam
import Database.Beam.Postgres
import Effects.Config (MonadConfig)
import Effects.Database as DB
import Effects.Log as Log
import Entities.Category
import Entities.Image
import Entities.News
import Entities.User
import Servant hiding (Tagged)

type NewsAPI =
  "list"
    :> Paginated
    :> SortableBy
         '[ "id",
            "title",
            "author",
            "created-at",
            "category",
            "is-published"
          ]
         ('Ascend "id")
    :> FilterableBy
         '[ 'Tagged "id" Int32,
            'Tagged "title" Text,
            'Tagged "author" Text,
            'Tagged "created-at" UTCTime,
            'Tagged "category" (CI Text),
            'Tagged "is-published" Bool
          ]
    :> Get '[JSON] [ArticleJSON]
    :<|> Capture "id" Int32 :> Get '[JSON] ArticleJSON
    :<|> Protected AuthorUser :> Capture "id" Int32 :> ReqBody '[JSON] ArticleUpdateJSON :> PostCreated '[JSON] ArticleJSON
    :<|> Protected AuthorUser :> ReqBody '[JSON] ArticlePostJSON :> PostCreated '[JSON] ArticleJSON

news :: ServerT NewsAPI App
news = listArticles :<|> getArticle :<|> updateArticle :<|> postArticle

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

data ArticlePostJSON = ArticlePostJSON
  { _articlePostJSONTitle :: Text,
    _articlePostJSONCategory :: Maybe (CI Text),
    _articlePostJSONBody :: Text,
    _articlePostJSONImages :: [FileNameJSON],
    _articlePostJSONIsPublished :: Bool
  }
  deriving (Show)

data ArticleUpdateJSON = ArticleUpdateJSON
  { _articleUpdateJSONTitle :: Maybe Text,
    _articleUpdateJSONCategory :: Maybe (CI Text),
    _articleUpdateJSONBody :: Maybe Text,
    _articleUpdateJSONImages :: Maybe [FileNameJSON],
    _articleUpdateJSONIsPublished :: Maybe Bool
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

instance A.FromJSON ArticlePostJSON where
  parseJSON = A.withObject "ArticleJSON" $ \o -> do
    _articlePostJSONTitle <- o A..: "title"
    _articlePostJSONCategory <- fmap CI.mk <$> o A..:? "category"
    _articlePostJSONBody <- o A..: "body"
    _articlePostJSONImages <- o A..:? "images" A..!= []
    _articlePostJSONIsPublished <- o A..:? "is-published" A..!= False
    pure ArticlePostJSON {..}

instance A.FromJSON ArticleUpdateJSON where
  parseJSON = A.withObject "ArticleUpdateJSON" $ \o -> do
    _articleUpdateJSONTitle <- o A..:? "title"
    _articleUpdateJSONCategory <- fmap CI.mk <$> o A..:? "category"
    _articleUpdateJSONBody <- o A..:? "body"
    _articleUpdateJSONImages <- o A..:? "images"
    _articleUpdateJSONIsPublished <- o A..:? "is-published"
    pure ArticleUpdateJSON {..}

newtype FileNameJSON = FileNameJSON {unFileNameJSON :: FileName}
  deriving (Show)

instance A.ToJSON FileNameJSON where
  toJSON (FileNameJSON fn) = A.String $ "/images/" <> T.tshow fn

instance A.FromJSON FileNameJSON where
  parseJSON = A.withText "FileNameJSON" (fmap FileNameJSON . parseFileName)

mkArticleJSON :: Article -> Text -> [FileName] -> ArticleJSON
mkArticleJSON Article {..} authorName imgs =
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

listArticles ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadConfig m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  Pagination ->
  SortingRequest
    '[ "id",
       "title",
       "author",
       "created-at",
       "category",
       "is-published"
     ]
    ('Ascend "id") ->
  FilteringRequest
    '[ 'Tagged "id" Int32,
       'Tagged "title" Text,
       'Tagged "author" Text,
       'Tagged "created-at" UTCTime,
       'Tagged "category" (CI Text),
       'Tagged "is-published" Bool
     ] ->
  m [ArticleJSON]
listArticles Pagination {..} sorting fReq = do
  Log.logInfo $ "Get /news sort-by=" <> T.tshow (unSortingRequest sorting)
  xs <-
    DB.runQuery
      . onlyAuthorName
      . runSelectReturningList
      . select
      . filterByRequest_ fReq filters
      . limit_ limit
      . offset_ offset
      . sortBy_ sorting sorters
      $ articleWithAuthor
  xs' <- mapM fetchImageNames xs
  pure $ fmap (\(article, authorName, imgs) -> mkArticleJSON article authorName imgs) xs'
  where
    onlyAuthorName = fmap (fmap (second _userName))
    fetchImageNames (article, authorName) = do
      imgs <-
        DB.runQuery
          . fmap (fmap (_imageIdFileName . snd))
          . runSelectReturningList
          . select
          $ articleImageReletaionship
            (_newsArticlesImages newsDB)
            (filter_ (\a -> _articleId a ==. val_ (_articleId article)) (all_ $ _newsArticles newsDB))
            (all_ $ _newsImages newsDB)
      return (article, authorName, imgs)
    articleWithAuthor = do
      article <- all_ $ _newsArticles newsDB
      user <- related_ (_newsUsers newsDB) (_articleAuthor article)
      pure (article, user)
    sorters (Article {..}, User {..}) =
      SortingApp
        ( sorterFor_ @"id" _articleId
            .:. sorterFor_ @"title" _articleTitle
            .:. sorterFor_ @"author" _userName
            .:. sorterFor_ @"created-at" _articleCreatedAt
            .:. sorterFor_ @"category" (unCategoryId _articleCategory)
            .:. sorterFor_ @"is-published" _articleIsPublished
            .:. ColNil
        )
    filters (Article {..}, User {..}) =
      FilteringApp
        ( filterFor_ @"id" _articleId
            .:. filterFor_ @"title" _articleTitle
            .:. filterFor_ @"author" _userName
            .:. filterFor_ @"created-at" _articleCreatedAt
            .:. filterFor_ @"category" (maybe_ (val_ $ CI.mk "") id $ unCategoryId _articleCategory)
            .:. filterFor_ @"is-published" _articleIsPublished
            .:. ColNil
        )

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
  (article, authorName) <- lookupArticleByIdWithAuthorName
  imgs <- DB.runQuery selectArticleImageFileNames
  pure $ mkArticleJSON article authorName imgs
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

postArticle ::
  ( MonadDatabase m,
    MonadIO m,
    MonadCatch m,
    MonadThrow m,
    MonadError ServerError m,
    MonadLog m
  ) =>
  User ->
  ArticlePostJSON ->
  m ArticleJSON
postArticle creator a@(ArticlePostJSON {..}) =
  flip catch dealWithAPIError $ do
    doLogRequest
    aM <- DB.runQuery $ insertArticle creator a
    case aM of
      Just art -> doLogSuccess art >> doOnSuccess art
      Nothing -> doLogFail >> throwError err500
  where
    articleT = _newsArticles newsDB
    imageT = _newsImages newsDB
    articleImageT = _newsArticlesImages newsDB
    dealWithAPIError err = case err of
      e@(APIError msg) -> Log.logWarning (T.tshow e) >> throwError err500 {errBody = T.textToLBS msg}
      other -> throwM other
    doOnSuccess art = do
      imgs <- DB.runQuery (selectArticleImageFileNames $ _articleId art)
      pure $ mkArticleJSON art (_userName creator) imgs
    selectArticleImageFileNames articleId =
      fmap _imageIdFileName <$> selectArticleImages articleT imageT articleImageT articleId
    doLogRequest = Log.logInfo $ "User: " <> _userName creator <> " posts new article: '" <> _articlePostJSONTitle <> "'"
    doLogSuccess art = Log.logInfo $ "User: " <> _userName creator <> " successfully posted new article, ID: " <> T.tshow (_articleId art)
    doLogFail = Log.logWarning $ "User: " <> _userName creator <> " failed to post article: '" <> _articlePostJSONTitle <> "'"

insertArticle ::
  (MonadBeam Postgres m, MonadBeam Postgres m) =>
  User ->
  ArticlePostJSON ->
  m (Maybe Article)
insertArticle creator (ArticlePostJSON {..}) = do
  runInsert . insert (_newsArticles newsDB) $
    insertExpressions
      [ Article
          { _articleId = default_,
            _articleTitle = val_ _articlePostJSONTitle,
            _articleCreatedAt = default_,
            _articleAuthor = UserId (val_ $ _userId creator),
            _articleCategory = CategoryId $ val_ _articlePostJSONCategory,
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
  let imgNames = unFileNameJSON <$> _articlePostJSONImages
  mapM_ (insertArticleImagesRelations imgNames) article
  pure article

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

updateArticle ::
  ( MonadDatabase m,
    MonadIO m,
    MonadCatch m,
    MonadThrow m,
    MonadError ServerError m,
    MonadLog m
  ) =>
  User ->
  Int32 ->
  ArticleUpdateJSON ->
  m ArticleJSON
updateArticle editor articleId aUpdate = flip catch dealWithAPIError $ do
  doLogRequest
  (article, author) <- lookupArticleByIdWithAuthorName
  let isAllowedToEdit = (_userId editor == _userId author && _userIsAllowedToPost editor) || _userIsAdmin editor
  if isAllowedToEdit
    then doUpdateArticle article
    else doLogUnauthorized >> throwError err401
  where
    articleT = _newsArticles newsDB
    usersT = _newsUsers newsDB
    imageT = _newsImages newsDB
    articleImageT = _newsArticlesImages newsDB
    articleIdText = T.tshow articleId
    lookupArticleByIdWithAuthorName = do
      xMaybe <- DB.runQuery . selectArticleWithAuthor articleT usersT $ ArticleId articleId
      case xMaybe of
        Nothing -> doLogNotFound >> throwError err404
        Just x -> pure x
    dealWithAPIError err = case err of
      e@(APIError msg) -> Log.logWarning (T.tshow e) >> throwError err500 {errBody = T.textToLBS msg}
      other -> throwM other
    doUpdateArticle article = do
      aM <- DB.runQuery (updateArticleDB article aUpdate)
      case aM of
        Just art -> doLogSuccess >> doOnSuccess art
        Nothing -> doLogFail >> throwError err500
    doOnSuccess art = do
      imgs <- DB.runQuery (selectArticleImageFileNames $ _articleId art)
      pure $ mkArticleJSON art (_userName editor) imgs
    selectArticleImageFileNames aId =
      fmap _imageIdFileName <$> selectArticleImages articleT imageT articleImageT aId
    doLogNotFound = Log.logInfo $ "Article " <> T.tshow articleId <> " not found"
    doLogRequest = Log.logInfo $ "User: " <> _userName editor <> " tries to modify article with ID: " <> articleIdText
    doLogSuccess = Log.logInfo $ "User: " <> _userName editor <> " successfully updated article with ID: " <> articleIdText
    doLogFail = Log.logWarning $ "User: " <> _userName editor <> " failed to modify article with ID: '" <> articleIdText
    doLogUnauthorized = Log.logWarning $ "User: " <> _userName editor <> " is not authorized to modify article with ID: '" <> articleIdText


updateArticleDB ::
  (MonadBeam Postgres m) =>
  Article ->
  ArticleUpdateJSON ->
  m (Maybe Article)
updateArticleDB article (ArticleUpdateJSON {..}) = do
  runUpdate $
    updateTable
      (_newsArticles newsDB)
      ( set
          { _articleTitle = toUpdatedVMaybe _articleUpdateJSONTitle,
            _articleBody = toUpdatedVMaybe _articleUpdateJSONBody,
            _articleCategory = CategoryId $ toUpdatedVMaybe $ Just <$> _articleUpdateJSONCategory,
            _articleIsPublished = toUpdatedVMaybe _articleUpdateJSONIsPublished
          }
      )
      ( \a ->
          _articleId a
            ==. val_ (_articleId article)
      )
  forM_
    _articleUpdateJSONImages
    (updateArticleImagesReleations article . fmap unFileNameJSON)
  runSelectReturningOne
    . select
    . filter_
      ( \a ->
          _articleId a
            ==. val_ (_articleId article)
      )
    $ all_ (_newsArticles newsDB)
  where
    toUpdatedVMaybe f = toUpdatedValueMaybe $ const (val_ <$> f)

updateArticleImagesReleations ::
  (MonadBeam Postgres m) =>
  Article ->
  [FileName] ->
  m ()
updateArticleImagesReleations article updatedImgNames = do
  imgs <- selectArticleImageFileNames $ _articleId article
  let imgsToRemove = filter (`notElem` updatedImgNames) imgs
      imgsToAdd = filter (`notElem` imgs) updatedImgNames
  runDelete $
    delete
      (_newsArticlesImages newsDB)
      ( \ai ->
          exists_ $
            articleImageReletaionship
              (_newsArticlesImages newsDB)
              (_newsArticles newsDB `related_` _articleImageArticleId ai)
              ( filter_
                  ( \i -> not_ (imgNameString i `in_` fmap (val_ . T.tshow) imgsToRemove)
                  )
                  (_newsImages newsDB `related_` _articleImageImageId ai)
              )
      )
  insertArticleImagesRelations imgsToAdd article
  where
    articleT = _newsArticles newsDB
    imageT = _newsImages newsDB
    articleImageT = _newsArticlesImages newsDB
    selectArticleImageFileNames articleId =
      fmap _imageIdFileName <$> selectArticleImages articleT imageT articleImageT articleId
    imgNameString i = concat_ [cast_ (_imageId i) (varchar Nothing), val_ ".", _imageFileExtension i]

