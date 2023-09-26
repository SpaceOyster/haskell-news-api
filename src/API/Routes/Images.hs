{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Images where

import API.Modifiers.Protected (Protected)
import App.Monad
import Control.Monad
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
    MonadError ServerError m
  ) =>
  Text ->
  m (Headers '[Header "Content-Type" String, Header "Content-Length" Integer] BS.ByteString)
getImage fileName = do
  (imageName, imageExt) <- parseImageFileName fileName
  imgContentM <-
    DB.runQuery
      . runSelectReturningOne
      . select
      . fmap _imageContent
      . filter_
        ( \i ->
            (_imageFileExtension i ==. val_ imageExt)
              &&. (cast_ (_imageId i) (varchar Nothing) ==. val_ imageName)
        )
      $ all_ (_newsImages newsDB)
  case imgContentM of
    Nothing -> throwError err404
    Just imgContent -> pure . addHeader (contMimeType imageExt) . addHeader (contLength imgContent) $ imgContent
  where
    contMimeType = imageExtensionToMimeType
    contLength = fromIntegral . BS.length

parseImageFileName ::
  ( MonadLog m,
    MonadError ServerError m
  ) =>
  Text ->
  m FileName
parseImageFileName fileName = case T.splitOn "." fileName of
  [imageId, imageExt] -> return $ FileName imageId imageExt
  _ -> Log.logInfo (T.tshow fileName <> " is invalid image name.") >> throwError err404

imageExtensionToMimeType :: Text -> String
imageExtensionToMimeType imageExt =
  T.unpack $
    "image/" <> case imageExt of
      "jpg" -> "jpeg"
      x -> x

newtype ImageJSON = ImageJSON Image

instance A.ToJSON ImageJSON where
  toJSON (ImageJSON (Image {..})) =
    let imgFileName = T.tshow _imageId <> "." <> _imageFileExtension
     in A.object
          [ "url" A..= ("/images/" <> imgFileName),
            "image-file-name" A..= imgFileName,
            "image-name" A..= _imageName
          ]

postImage ::
  ( Monad m,
    MonadDatabase m,
    MonadLog m,
    MonadError ServerError m
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
    fileToImageData file = do
      let fileName = fdFileName file
          newImageDataContent = LBS.toStrict $ fdPayload file
          newImageMimeType = fdFileCType file
      (fnName, fnExtension) <- parseImageFileName fileName
      Log.logInfo $ "Form: " <> fdInputName file <> " Got file: " <> fileName
      let newImageFileName = FileName {..}
      return $ NewImage {..}
    doOnUnauthorised = doLogUnauthorised >> throwError err401
    doLogDBError fname =
      Log.logWarning $
        "Image \"" <> fname <> "\" was not added to Database"
    doLogUnauthorised =
      Log.logWarning $
        "User \"" <> creatorLogin <> "\" is not authorised to create a new image"
