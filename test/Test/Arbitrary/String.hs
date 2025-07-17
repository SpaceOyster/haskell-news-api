module Test.Arbitrary.String where

import Test.QuickCheck
  ( Arbitrary (arbitrary),
    elements,
    listOf,
    listOf1,
    resize,
    suchThat,
  )

alphaChars :: [Char]
alphaChars = ['a' .. 'z'] <> ['A' .. 'Z']

numChars :: [Char]
numChars = ['0' .. '9']

alphaNumChars :: [Char]
alphaNumChars = alphaChars <> numChars

unreservedURIChars :: [Char]
unreservedURIChars = alphaNumChars <> "-_.~"

newtype AlphaNumString = AlphaNumString {getAlphaNumString :: String}
  deriving Show

instance Arbitrary AlphaNumString where
  arbitrary = AlphaNumString <$> listOf (elements alphaNumChars)

newtype CleanString = CleanString {getCleanString :: String}
  deriving (Show)

instance Arbitrary CleanString where
  arbitrary = do
    let allowedChars = unreservedURIChars
    CleanString <$> listOf (elements allowedChars)

newtype ShortCleanString = ShortCleanString {getShortCleanString :: String}

instance Arbitrary ShortCleanString where
  arbitrary = ShortCleanString <$> resize 10 arbitrary

newtype NonEmptyCleanString = NonEmptyCleanString {getNonEmptyCleanString :: String}
  deriving (Show)

instance Arbitrary NonEmptyCleanString where
  arbitrary = do
    let allowedChars = unreservedURIChars
    NonEmptyCleanString <$> listOf1 (elements allowedChars)

newtype FileNameString = FileNameString {getFileNameString :: String}
  deriving (Show)

newtype FileExtString = FileExtString {getFileExtString :: String}
  deriving (Show)

newtype FileNameExtString = FileNameExtString {getFileNameExtString :: String}
  deriving (Show)

instance Arbitrary FileNameString where
  arbitrary = do
    let allowedChars = alphaNumChars <> "-_~!@#$%^&()=+,[]{}"
    FileNameString <$> listOf (elements allowedChars)

instance Arbitrary FileExtString where
  arbitrary = FileExtString . getAlphaNumString <$> arbitrary

instance Arbitrary FileNameExtString where
  arbitrary = do
    fName <- getFileNameString <$> arbitrary
    fExt <- getFileExtString <$> arbitrary
    pure . FileNameExtString . mconcat $ [fName, ".", fExt]
