{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Entities.User where

import qualified Data.ByteString as BS
import Data.Int
import Data.Text
import Data.Time.Clock
import Database.Beam

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
