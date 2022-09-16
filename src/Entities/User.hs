{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.User where

import qualified Crypto.KDF.PBKDF2 as Crypto
import qualified Crypto.Random as CRand
import qualified Data.ByteString as BS
import Data.Int
import Data.Text
import Data.Text.Encoding as T
import Data.Time.Clock
import Database.Beam
import Database.Beam.Postgres
import Effects.Database
import qualified System.Random as Rand

data UserT f = User
  { _userId :: Columnar f Int32,
    _userName :: Columnar f Text,
    _userLogin :: Columnar f Text,
    _userPasswordHash :: Columnar f BS.ByteString,
    _userPasswordHashIterations :: Columnar f Int32,
    _userPasswordSalt :: Columnar f BS.ByteString,
    _userRegistrationDate :: Columnar f UTCTime,
    _userIsAdmin :: Columnar f Bool,
    _userIsAllowedToPost :: Columnar f Bool
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId

type UserId = PrimaryKey UserT Identity

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

data PasswordHash = PasswordHash
  { _passwordHash :: BS.ByteString,
    _passwordHashIterations :: Int32,
    _passwordHashSalt :: BS.ByteString
  }
  deriving (Show)

generateHash :: MonadIO m => BS.ByteString -> m PasswordHash
generateHash pwd = liftIO $ do
  hashLength <- Rand.getStdRandom $ Rand.randomR (32, 64)
  hashSaltLength <- Rand.getStdRandom $ Rand.randomR (8, 32)
  _passwordHashIterations <- Rand.getStdRandom $ Rand.randomR (1000, 4000)
  let params =
        Crypto.Parameters
          { iterCounts = fromIntegral _passwordHashIterations,
            outputLength = hashLength
          }
  _passwordHashSalt <- CRand.getRandomBytes hashSaltLength
  let _passwordHash = Crypto.fastPBKDF2_SHA256 params pwd _passwordHashSalt
  return $ PasswordHash {..}

data NewUserCredentials = NewUser
  { _newUserName :: Text,
    _newUserLogin :: Text,
    _newUserPassword :: Text,
    _newUserIsAdmin :: Bool,
    _newUserIsAllowedToPost :: Bool
  }
  deriving (Show)

insertNewUser ::
  (MonadDatabase m, MonadIO m) =>
  DatabaseEntity Postgres db (TableEntity UserT) ->
  NewUserCredentials ->
  m ()
insertNewUser table NewUser {..} = do
  PasswordHash {..} <- generateHash $ T.encodeUtf8 _newUserPassword
  runQuery . runInsert . insert table $
    insertExpressions
      [ User
          { _userId = default_,
            _userName = val_ _newUserName,
            _userLogin = val_ _newUserLogin,
            _userPasswordHash = val_ _passwordHash,
            _userPasswordHashIterations = val_ _passwordHashIterations,
            _userPasswordSalt = val_ _passwordHashSalt,
            _userRegistrationDate = default_,
            _userIsAdmin = val_ _newUserIsAdmin,
            _userIsAllowedToPost = val_ _newUserIsAllowedToPost
          }
      ]
