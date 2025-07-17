module Test.Arbitrary.Text where

import Data.Text as T (Text, pack)
import Test.Arbitrary.String
  ( CleanString (getCleanString),
    NonEmptyCleanString (getNonEmptyCleanString), getFileNameString, getFileExtString,
  )
import Test.QuickCheck (Arbitrary (arbitrary), NonEmptyList (getNonEmpty))

newtype AnyText = AnyText {getAnyText :: Text}
  deriving (Show)

instance Arbitrary AnyText where
  arbitrary = AnyText . T.pack <$> arbitrary

newtype NonEmptyText = NonEmptyText {getNonEmptyText :: Text}
  deriving (Show)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack . getNonEmpty <$> arbitrary

newtype CleanText = CleanText {getCleanText :: Text}
  deriving (Show)

instance Arbitrary CleanText where
  arbitrary = CleanText . T.pack . getCleanString <$> arbitrary

newtype ShortCleanText = ShortCleanText {getShortCleanText :: Text}
  deriving (Show)

instance Arbitrary ShortCleanText where
  arbitrary = ShortCleanText . T.pack . getCleanString <$> arbitrary

newtype NonEmptyCleanText = NonEmptyCleanText {getNonEmptyCleanText :: Text}
  deriving (Show)

instance Arbitrary NonEmptyCleanText where
  arbitrary = NonEmptyCleanText . T.pack . getNonEmptyCleanString <$> arbitrary

newtype FileNameText = FileNameText {getFileNameText :: Text}
  deriving (Show)

newtype FileExtText = FileExtText {getFileExtText :: Text}
  deriving (Show)

newtype FileNameExtText = FileNameExtText {getFileNameExtText :: Text}
  deriving (Show)

instance Arbitrary FileNameText where
  arbitrary = FileNameText . T.pack 
    . getFileNameString <$> arbitrary

instance Arbitrary FileExtText where
  arbitrary = FileExtText . T.pack 
    . getFileExtString <$> arbitrary

instance Arbitrary FileNameExtText where
  arbitrary = do
    fName <- getFileNameText <$> arbitrary
    fExt <- getFileExtText <$> arbitrary
    pure . FileNameExtText . mconcat $ [fName, T.pack ".", fExt]
