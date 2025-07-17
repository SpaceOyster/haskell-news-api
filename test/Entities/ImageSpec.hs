{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.ImageSpec (spec) where

import Control.Monad.State.Lazy
import Data.Text.Extended as T
import Entities.Image
import Test.Arbitrary.Text
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Entities.Image" $ do
  _imageFileNameSpec
  _imageIdFileNameSpec
  parseFileNameSpec
  newImageNameSpec
  newImageDataExtensionSpec

emptyImage :: Image
emptyImage =
  Image
    { _imageId = 0,
      _imageMimeType = mempty,
      _imageContent = mempty,
      _imageName = mempty,
      _imageFileExtension = mempty
    }

_imageFileNameSpec :: Spec
_imageFileNameSpec = describe "_imageFileName" $
  context "_imageName and _imageFileExtension strings should be clean, otherwise result is not guaranteed" $
    prop "accepts Image and returns FileName based on _imageName" $
      \(fName, fExt) -> do
        let img =
              emptyImage
                { _imageName = getFileNameText fName,
                  _imageFileExtension = getFileExtText fExt
                }
        let filename =
              FileName
                { fnName = getFileNameText fName,
                  fnExtension = getFileExtText fExt
                }
        _imageFileName img `shouldBe` filename

_imageIdFileNameSpec :: Spec
_imageIdFileNameSpec = describe "_imageIdFileName" $ 
  context "_imageFileExtension string should be clean, otherwise result is not guaranteed" $
    prop "accepts Image and returns FileName based on _imageId" $
      \(fId, fExt) -> do
        let img =
              emptyImage
                { _imageId = fId,
                  _imageFileExtension = getFileExtText fExt
                }
        let filename =
              FileName
                { fnName = T.tshow fId,
                  fnExtension = getFileExtText fExt
                }
        _imageIdFileName img `shouldBe` filename

parseFileNameSpec :: Spec
parseFileNameSpec = describe "parseFileName" $
  context "The parsed string should have only on '.' symbol, otherwise result is not guaranteed" $ do
    prop "accepts Text with one '.' and returns FileName" $
      \(fName, fExt) -> do
        let fText = getFileNameText fName <> "." <> getFileExtText fExt
        let filename = FileName {
            fnName = getFileNameText fName,
            fnExtension = getFileExtText fExt
          }
        parseFileName fText `shouldBe` Just filename
    prop "Text with more than one '.' results in failure" $
      \(fName, fExt) -> do
        let fText = getFileNameText fName <> "." <> getFileExtText fExt <> "." <> getFileExtText fExt
        parseFileName fText `shouldBe` Nothing

emptyNewImage = NewImage {
    newImageFileName = FileName {fnName = mempty, fnExtension = mempty},
    newImageMimeType = mempty,
    newImageDataContent = mempty
  }

newImageNameSpec :: Spec
newImageNameSpec = describe "newImageNameSpec" $
  prop "simple file name getter, returns fnName of NewImage FileName" $
    \fName -> do
      let fn = FileName {fnName = getFileNameText fName, fnExtension = mempty}
      let img = emptyNewImage {newImageFileName = fn}
      newImageFileName img `shouldBe` fn


newImageDataExtensionSpec :: Spec
newImageDataExtensionSpec = describe "newImageDataExtensionSpec" $
  prop "simple file extension getter, returns fnExt of NewImage FileName" $
    \fExt -> do
      let fn = FileName {fnName = mempty, fnExtension = getFileNameText fExt}
      let img = emptyNewImage {newImageFileName = fn}
      newImageFileName img `shouldBe` fn
