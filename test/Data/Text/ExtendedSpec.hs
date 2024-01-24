{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ExtendedSpec (spec) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import Data.String
import qualified Data.Text.Extended as T
import Test.Arbitrary.String
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Data.Text.Extended" $ do
  tshowSpec
  textToLBSSpec

tshowSpec :: Spec
tshowSpec = describe "tshow" $ do
  it "converts type of Show class to Text" $ do
    T.tshow (Just ("some" :: String)) `shouldBe` ("Just \"some\"" :: T.Text)
    T.tshow (Just [1 :: Integer, 2, 3, 4]) `shouldBe` ("Just [1,2,3,4]" :: T.Text)
    T.tshow [1 :: Integer, 2, 3, 4] `shouldBe` ("[1,2,3,4]" :: T.Text)
    T.tshow (1234 :: Integer) `shouldBe` ("1234" :: T.Text)

textToLBSSpec :: Spec
textToLBSSpec = describe "textToLBS" $ do
  context "doesn't change clean strings, not all strings are guarantied to remain unchanged" $
    prop "converts Text to UTF8 Lazy ByteString" $
      \str -> T.textToLBS (T.pack $ getCleanString str) `shouldBe` LBS.fromStrict (BSI.packChars (getCleanString str))
