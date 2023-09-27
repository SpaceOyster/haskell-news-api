{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Images where

import API.Modifiers.Protected (Protected)
import App.Error (apiError)
import App.Monad
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.CaseInsensitive as CI
import Data.Text.Extended as T
import Database.Beam
import Effects.Database as DB
import Effects.Log as Log
import Entities.Image
import Entities.User
import Servant
import Servant.Multipart as MP

type ImagesAPI =
  Capture "imageName" Text :> Get '[OctetStream] (Headers '[Header "Content-Type" String, Header "Content-Length" Integer] BS.ByteString)
    :<|> Protected :> MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] [ImageJSON]

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
getImage fileNameT = do
  Log.logInfo $ "Image requested: " <> fileNameT
  fileName <- parseFileName fileNameT
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

parseFileName :: (MonadThrow m) => Text -> m FileName
parseFileName fileName = case T.splitOn "." fileName of
  [imageId, imageExt] -> return $ FileName imageId imageExt
  _ -> throwM $ apiError (T.tshow fileName <> " is invalid file name.")

fileToImageData :: (MonadThrow m) => FileData Mem -> m NewImage
fileToImageData file = do
  let fileName = fdFileName file
      newImageDataContent = LBS.toStrict $ fdPayload file
      newImageMimeType = fdFileCType file
  newImageFileName <- parseFileName fileName
  return $ NewImage {..}

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
postImage usr multipartData =
  if _userIsAdmin usr then doCreateImage else doOnUnauthorised
  where
    table = _newsCategories newsDB
    creatorLogin = CI.original (_userLogin usr)
    doCreateImage = do
      let files = MP.files multipartData
      newImgsData <- forM files fileToImageData
      insertNewImages (_newsImages newsDB) newImgsData
      imgs <- selectImages (_newsImages newsDB) . fmap newImageFileName $ newImgsData
      return $ ImageJSON <$> imgs
    doOnUnauthorised = doLogUnauthorised >> throwError err401
    doLogDBError fname =
      Log.logWarning $
        "Image \"" <> fname <> "\" was not added to Database"
    doLogUnauthorised =
      Log.logWarning $
        "User \"" <> creatorLogin <> "\" is not authorised to create a new image"
