{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Images where

import API.Modifiers.Protected (AuthorUser, Protected)
import App.Error (AppError (APIError), apiError)
import App.Monad
import Control.Monad
import Control.Monad.Catch (MonadCatch (catch), MonadThrow, throwM)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS (fromStrict, toStrict)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Extended as T
import Database.Beam
import Effects.Database as DB
import Effects.Log as Log
import Entities.Image
import Entities.User
import Servant
import Servant.Multipart as MP

type ImagesAPI =
  Capture "imageName" Text :> Get '[OctetStream] (Headers '[Header "Content-Type" String, Header "Content-Length" Integer] BS.ByteString)
    :<|> Protected AuthorUser :> MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] [ImageJSON]

images :: ServerT ImagesAPI App
images = getImage :<|> postImage

-- TODO: 'Content-Type' header is duplicated
getImage ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m,
    MonadCatch m
  ) =>
  Text ->
  m (Headers '[Header "Content-Type" String, Header "Content-Length" Integer] BS.ByteString)
getImage fileNameT = flip catch dealWithAPIError $ do
  Log.logInfo $ "Image requested: " <> fileNameT
  fileName <- parseFileName' fileNameT -- TODO: throw err404 here
  imgMaybe <- selectImage (_newsImages newsDB) fileName
  case imgMaybe of
    Nothing -> doLogNotFound >> throwError err404
    Just img -> doLogFound >> doReturnImage img
  where
    contLength = fromIntegral . BS.length
    doReturnImage Image {..} =
      pure
        . addHeader (T.unpack _imageMimeType)
        . addHeader (contLength _imageContent)
        $ _imageContent
    dealWithAPIError e = case e of
      a@(APIError msg) -> Log.logWarning (T.tshow a) >> throwError err500 {errBody = T.textToLBS msg}
      other -> throwM other
    doLogNotFound = Log.logInfo $ "Image \"" <> fileNameT <> "\" not found"
    doLogFound = Log.logInfo $ "Image \"" <> fileNameT <> "\" found and returned"

newtype ImageJSON = ImageJSON Image

instance A.ToJSON ImageJSON where
  toJSON (ImageJSON (Image {..})) =
    let imgFileName = T.tshow _imageId <> "." <> _imageFileExtension
     in A.object
          [ "url" A..= ("/images/" <> imgFileName),
            "image-file-name" A..= imgFileName,
            "image-name" A..= _imageName
          ]

parseFileName' :: (MonadThrow m) => Text -> m FileName
parseFileName' fileName = maybe throw pure $ parseFileName fileName
  where
    throw = throwM $ apiError (T.tshow fileName <> " is invalid file name.")

fileToNewImage :: (MonadThrow m) => FileData Mem -> m NewImage
fileToNewImage file = do
  let fileName = fdFileName file
      newImageDataContent = LBS.toStrict $ fdPayload file
      newImageMimeType = fdFileCType file
  newImageFileName <- parseFileName' fileName
  pure $ NewImage {..}

postImage ::
  ( Monad m,
    MonadDatabase m,
    MonadLog m,
    MonadError ServerError m,
    MonadCatch m
  ) =>
  User ->
  MultipartData Mem ->
  m [ImageJSON]
postImage creator multipartData =
  doCreateImage
  where
    table = _newsCategories newsDB
    creatorLogin = CI.original (_userLogin creator)
    doCreateImage = flip catch dealWithAPIError $ do
      let files = MP.files multipartData
      newImgsData <- forM files fileToNewImage
      insertNewImages (_newsImages newsDB) newImgsData
      imgs <- selectImages (_newsImages newsDB) $ newImageFileName <$> newImgsData
      doCheckForSuccess newImgsData
    dealWithAPIError e = case e of
      a@(APIError msg) -> Log.logWarning (T.tshow a) >> throwError err500 {errBody = T.textToLBS msg}
      other -> throwM other
    doCheckForSuccess newImgs = do
      imgs <- selectImages (_newsImages newsDB) $ newImageFileName <$> newImgs
      if length imgs == length newImgs
        then doLogSuccess newImgs >> pure (ImageJSON <$> imgs)
        else doLogDBError newImgs >> throwError err503
    doLogDBError newImgs =
      Log.logWarning $
        "Some Images of " <> T.tshow (newImageFileName <$> newImgs) <> " were not added to Database"
    doLogSuccess newImgs =
      Log.logInfo $
        "User \"" <> creatorLogin <> "\" created new images: " <> T.tshow (newImageFileName <$> newImgs)
