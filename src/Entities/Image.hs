{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.Image where

import Data.ByteString
import Data.Int
import Data.Text
import Database.Beam
import Database.Beam.Postgres
import Effects.Database as DB

data ImageT f = Image
  { _imageId :: Columnar f Int32,
    _imageName :: Columnar f Text,
    _imageFileExtension :: Columnar f Text,
    _imageMimeType :: Columnar f Text,
    _imageContent :: Columnar f ByteString
  }
  deriving (Generic, Beamable)

type Image = ImageT Identity

deriving instance Show Image

deriving instance Eq Image

instance Table ImageT where
  data PrimaryKey ImageT f = ImageId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = ImageId . _imageId

type ImageId = PrimaryKey ImageT Identity

deriving instance Show (PrimaryKey ImageT Identity)

deriving instance Eq (PrimaryKey ImageT Identity)

data FileName = FileName {fnName :: Text, fnExtension :: Text}

data NewImage = NewImage
  { newImageFileName :: FileName,
    newImageMimeType :: Text,
    newImageDataContent :: ByteString
  }

newImageName :: NewImage -> Text
newImageName = fnName . newImageFileName

newImageDataExtension :: NewImage -> Text
newImageDataExtension = fnExtension . newImageFileName

insertNewImages ::
  (MonadDatabase m) =>
  DatabaseEntity Postgres db (TableEntity ImageT) ->
  [NewImage] ->
  m ()
insertNewImages table imagesData = do
  runQuery . runInsert . insert table $
    insertExpressions (imgDataToExpr <$> imagesData)
  where
    imgDataToExpr ni@(NewImage {..}) =
      Image
        { _imageId = default_,
          _imageName = val_ $ newImageName ni,
          _imageMimeType = val_ newImageMimeType,
          _imageFileExtension = val_ $ newImageDataExtension ni,
          _imageContent = val_ newImageDataContent
        }

selectImage ::
  (MonadDatabase m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity ImageT) ->
  FileName ->
  m (Maybe Image)
selectImage table fn =
  DB.runQuery
    . runSelectReturningOne
    . select
    . filter_
      ( \i ->
          (_imageFileExtension i ==. val_ (fnExtension fn))
            &&. (cast_ (_imageId i) (varchar Nothing) ==. val_ (fnName fn))
      )
    $ all_ table

selectImages ::
  (MonadDatabase m, Database Postgres db) =>
  DatabaseEntity Postgres db (TableEntity ImageT) ->
  [FileName] ->
  m [Image]
selectImages table fns =
  DB.runQuery
    . runSelectReturningList
    . select
    . filter_
      ( \i ->
          (_imageFileExtension i `in_` (val_ . fnExtension <$> fns))
            &&. (_imageName i `in_` (val_ . fnName <$> fns))
      )
    $ all_ table
