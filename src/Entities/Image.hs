{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.Image where

import Data.ByteString
import Data.Int
import Data.Text
import Database.Beam

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
