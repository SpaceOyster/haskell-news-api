{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Images where

import App.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.ByteString as BS
import Data.Maybe
import Data.Text.Extended as T
import Database.Beam
import Effects.Database as DB
import Effects.Log as Log
import Entities.Image
import Servant

type ImagesAPI =
  Capture "imageName" Text :> Get '[OctetStream] (Headers '[Header "Content-Type" String, Header "Content-Length" Integer] BS.ByteString)
    :<|> PostCreated '[JSON] Text

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
  m (Text, Text)
parseImageFileName fileName = case T.splitOn "." fileName of
  [imageId, imageExt] -> return (imageId, imageExt)
  _ -> Log.logInfo (T.tshow fileName <> " is invalid image name.") >> throwError err404

imageExtensionToMimeType :: Text -> String
imageExtensionToMimeType imageExt =
  T.unpack $
    "image/" <> case imageExt of
      "jpg" -> "jpeg"
      x -> x

postImage multipartData = return "POST categories endpoint"
newtype ImageJSON = ImageJSON Image

instance A.ToJSON ImageJSON where
  toJSON (ImageJSON (Image {..})) =
    let imgFileName = T.tshow _imageId <> "." <> _imageFileExtension
     in A.object
          [ "url" A..= ("/images/" <> imgFileName),
            "image-file-name" A..= imgFileName,
            "image-name" A..= _imageName
          ]

