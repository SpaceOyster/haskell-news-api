{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.UserSpec where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BCH
import Data.Time.Clock
import Entities.User
import GHC.Natural
import Test.Arbitrary.String
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers

spec :: Spec
spec = describe "Entities.Image" $ do
  generateHashSpec
  checkPasswordSpec

generateHashSpec :: Spec
generateHashSpec = describe "generateHash" $
  prop "resulting PasswordHash is guarannteed to be random" $ do
    \pwd -> do
      hash1 <- generateHash $ BS.pack pwd
      hash2 <- generateHash $ BS.pack pwd
      hash1 == hash2 `shouldBe` False

emptyUserWithPHash :: PasswordHash -> User
emptyUserWithPHash hash =
  User
    { _userId = 0,
      _userName = mempty,
      _userLogin = mempty,
      _userPasswordHash = _passwordHash hash,
      _userPasswordHashIterations = _passwordHashIterations hash,
      _userPasswordSalt = _passwordHashSalt hash,
      _userRegistrationDate = UTCTime (toEnum 0) (toEnum 0),
      _userIsAdmin = False,
      _userIsAllowedToPost = False
    }

alterStringSymbolAt :: Natural -> String -> String
alterStringSymbolAt ix' str = zipWith ($) vector str
  where
    strLength = length str
    ix = fromIntegral ix' `rem` strLength
    alteringFunction = succ
    vector = replicate ix id <> (alteringFunction : replicate (strLength - ix + 1) id)

checkPasswordSpec :: Spec
checkPasswordSpec = describe "checkPassword" $
  context "recieves non empty password String and User data, no guaranties for empty password String" $ do
    prop "returns True if password is correct" $
      \(NonEmpty pwd) -> do
        hash <- generateHash $ BS.pack pwd
        let usr = emptyUserWithPHash hash
        checkPassword (BS.pack pwd) usr `shouldBe` True
    prop "minor difference in password results in False" $
      \(NonEmpty pwd, NonNegative ix') -> do
        hash <- generateHash $ BCH.pack pwd
        let usr = emptyUserWithPHash hash
        let slightlyAlteredPwd = BCH.pack (alterStringSymbolAt (fromIntegral @Int ix') pwd)
        checkPassword slightlyAlteredPwd usr `shouldBe` False
