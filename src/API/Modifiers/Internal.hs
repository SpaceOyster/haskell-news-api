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
import GHC.TypeLits

data Tagged t = Tagged Symbol t

newtype TaggedColumn (tag :: Symbol) be s a = TaggedCol
  { unTaggedCol :: QExpr be s a
  }

data ColumnList be s a where
  ColNil :: ColumnList be s '[]
  ColCons :: TaggedColumn tag be s a -> ColumnList be s as -> ColumnList be s ('Tagged tag a ': as)

(.:.) :: TaggedColumn tag be s a -> ColumnList be s as -> ColumnList be s ('Tagged tag a : as)
(.:.) = ColCons

infixr 3 .:.

infixr 3 `ColCons`

symbolText :: (KnownSymbol a) => Proxy a -> T.Text
symbolText = T.pack . symbolVal

symbolCIText :: (KnownSymbol a) => Proxy a -> CI T.Text
symbolCIText = CI.mk . symbolText

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

type family HasToBeInList (a :: Symbol) (cols :: [Symbol]) :: Constraint where
  HasToBeInList a cols =
    If
      (Elem a cols)
      (() :: Constraint)
      ( TypeError
          ( 'Text "Symbol '"
              ':<>: 'ShowType a
              ':<>: 'Text "' is not present in '"
              ':<>: 'ShowType cols
              ':<>: 'Text "' type"
          )
      )

type family IsSubset (a :: [k]) (b :: [k]) :: Bool where
  IsSubset a a = 'True
  IsSubset (a ': as) bs = Elem a bs && IsSubset as bs

type family HasToBeSubset (a :: [k]) (b :: [k]) :: Constraint where
  HasToBeSubset a b =
    If
      (IsSubset a b)
      (() :: Constraint)
      ( TypeError
          ( 'Text "List '"
              ':<>: 'ShowType a
              ':<>: 'Text "' is not a subset of list '"
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
