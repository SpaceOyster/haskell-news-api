{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Internal.PolyKinds where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Extended as T
import Data.Type.Bool (If, type (&&))
import Data.Typeable (Proxy (..))
import GHC.Base (Constraint, Symbol)
import GHC.TypeLits.Extended
  ( ErrorMessage (ShowType, Text, (:<>:)),
    KnownSymbol,
    TypeError,
    symbolCIText,
    symbolVal,
  )

type family Elem (a :: k) (l :: [k]) :: Bool where
  Elem a '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (_ ': as) = Elem a as

infix 3 `Elem`

type family HasToBeInList (a :: k) (cols :: [k]) :: Constraint where
  HasToBeInList a cols =
    If
      (Elem a cols)
      (() :: Constraint)
      ( TypeError
          ( 'Text "Type literal '"
              ':<>: 'ShowType a
              ':<>: 'Text "' is not present in '"
              ':<>: 'ShowType cols
              ':<>: 'Text "' type"
          )
      )

type family IsSubset (a :: [k]) (b :: [k]) :: Bool where
  IsSubset '[] _ = 'True
  IsSubset a a = 'True
  IsSubset (a ': as) bs = Elem a bs && IsSubset as bs

type family HasToBeSubset (a :: [k]) (b :: [k]) :: Constraint where
  HasToBeSubset a b =
    If
      (IsSubset a b)
      (() :: Constraint)
      ( TypeError
          ( 'Text "List "
              ':<>: 'ShowType a
              ':<>: 'Text " is not a subset of list "
              ':<>: 'ShowType b
          )
      )

class ReifyBool (b :: Bool) where
  reifyBool :: Proxy b -> Bool

instance ReifyBool 'True where
  reifyBool _ = True

instance ReifyBool 'False where
  reifyBool _ = False

class ValidNamesList (available :: [Symbol]) where
  isNameValid :: T.Text -> Bool

instance ValidNamesList '[] where
  isNameValid _ = False

instance (KnownSymbol a, ValidNamesList as) => ValidNamesList (a ': as) where
  isNameValid n = (symbolCIText (Proxy @a) == CI.mk n) || isNameValid @as n

class ReifySymbolsList a where
  reifySymbolsList :: Proxy a -> [String]

instance ReifySymbolsList '[] where
  reifySymbolsList _ = []

instance
  (KnownSymbol a, ReifySymbolsList as) =>
  ReifySymbolsList (a ': as)
  where
  reifySymbolsList _ = symbolVal (Proxy @a) : reifySymbolsList (Proxy @as)
