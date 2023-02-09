{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Internal where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Extended as T
import Data.Type.Bool
import Data.Typeable
import Data.Void
import Database.Beam.Query (QExpr)
import GHC.Base
import GHC.TypeLits.Extended

data Tagged t = Tagged Symbol t

newtype TaggedColumn (tag :: Symbol) be s a = TaggedCol
  { unTaggedCol :: QExpr be s a
  }

type family ListOfTags (a :: [Tagged Type]) :: [Symbol] where
  ListOfTags '[] = '[]
  ListOfTags ('Tagged tag a ': as) = tag ': ListOfTags as

data ColumnList be s a where
  ColNil :: ColumnList be s '[]
  ColCons :: TaggedColumn tag be s a -> ColumnList be s as -> ColumnList be s ('Tagged tag a ': as)

(.:.) :: TaggedColumn tag be s a -> ColumnList be s as -> ColumnList be s ('Tagged tag a : as)
(.:.) = ColCons

infixr 3 .:.

infixr 3 `ColCons`


class LookupColumn be s a | a -> be s where
  lookupColumn :: a -> CI T.Text -> Maybe (QExpr be s Void)

instance LookupColumn be s (ColumnList be s '[]) where
  lookupColumn _ _ = Nothing

instance
  ( KnownSymbol tag,
    LookupColumn be s (ColumnList be s as)
  ) =>
  LookupColumn be s (ColumnList be s ('Tagged tag a ': as))
  where
  lookupColumn (TaggedCol c `ColCons` as) name =
    if symbolCIText (Proxy @tag) == name
      then Just $ coerce c
      else lookupColumn as name

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

type family GetColumnTags a :: [Symbol] where
  GetColumnTags (ColumnList be s '[]) = '[]
  GetColumnTags (ColumnList be s ('Tagged tag a ': as)) =
    tag ': GetColumnTags (ColumnList be s as)

type family ColumnIsPresent (a :: Symbol) as :: Bool where
  ColumnIsPresent a (ColumnList be s l) =
    a `Elem` GetColumnTags (ColumnList be s l)

infix 3 `ColumnIsPresent`

type family HasToBeProvided (a :: Symbol) cols :: Constraint where
  HasToBeProvided a cols =
    If
      (a `ColumnIsPresent` cols)
      (() :: Constraint)
      ( TypeError
          ( 'Text "Column '"
              ':<>: 'ShowType a
              ':<>: 'Text "' is not present in '"
              ':<>: 'ShowType cols
              ':<>: 'Text "' type"
          )
      )

class ValidNamesList (available :: [Symbol]) where
  isNameValid :: CI T.Text -> Bool

instance ValidNamesList '[] where
  isNameValid _ = False

instance (KnownSymbol a, ValidNamesList as) => ValidNamesList (a ': as) where
  isNameValid n = (symbolCIText (Proxy @a) == n) || isNameValid @as n
