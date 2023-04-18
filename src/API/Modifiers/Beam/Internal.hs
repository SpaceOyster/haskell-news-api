{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Beam.Internal where

import API.Modifiers.Internal
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Extended as T
import Data.Type.Bool (If)
import Data.Typeable (Proxy (..))
import Data.Void (Void)
import Database.Beam.Query (QExpr)
import GHC.Base (Constraint, coerce)
import GHC.TypeLits.Extended

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

class LookupColumn' be s a typ | typ a -> be s where
  lookupColumn' :: a -> T.Text -> Maybe (QExpr be s typ)

instance LookupColumn' be s (ColumnList be s '[]) typ where
  lookupColumn' _ _ = Nothing

instance
  ( KnownSymbol tag,
    LookupColumn' be s (ColumnList be s as) typ
  ) =>
  LookupColumn' be s (ColumnList be s ('Tagged tag typ ': as)) typ
  where
  lookupColumn' (TaggedCol c `ColCons` as) name =
    if symbolCIText (Proxy @tag) == CI.mk name
      then Just c
      else lookupColumn' as name

class LookupColumn be s a | a -> be s where
  lookupColumn :: a -> T.Text -> Maybe (QExpr be s Void)

instance LookupColumn be s (ColumnList be s '[]) where
  lookupColumn _ _ = Nothing

instance
  ( KnownSymbol tag,
    LookupColumn be s (ColumnList be s as)
  ) =>
  LookupColumn be s (ColumnList be s ('Tagged tag a ': as))
  where
  lookupColumn (TaggedCol c `ColCons` as) name =
    if symbolCIText (Proxy @tag) == CI.mk name
      then Just $ coerce c
      else lookupColumn as name

type family GetColumnTags a :: [Symbol] where
  GetColumnTags (ColumnList be s '[]) = '[]
  GetColumnTags (ColumnList be s ('Tagged tag a ': as)) =
    tag ': GetColumnTags (ColumnList be s as)

type family ColumnIsPresent (a :: Symbol) as :: Bool where
  ColumnIsPresent a (ColumnList be s l) =
    a `Elem` GetColumnTags (ColumnList be s l)

infix 3 `ColumnIsPresent`

type family HasToBeProvided (a :: Symbol) cols :: Constraint where
  HasToBeProvided a (ColumnList be s as) =
    If
      (a `ColumnIsPresent` ColumnList be s as)
      (() :: Constraint)
      ( TypeError
          ( 'Text "Column '"
              ':<>: 'ShowType a
              ':<>: 'Text "' is not present in '"
              ':<>: 'ShowType (ColumnList be s as)
              ':<>: 'Text "' type"
          )
      )

class ObtainColumn' be s a (tag :: Symbol) typ | a tag -> be s where
  obtainColumn' :: a -> Proxy tag -> Proxy typ -> QExpr be s typ

instance
  {-# OVERLAPPABLE #-}
  forall be s a as typ (tag :: Symbol) (tag' :: Symbol).
  ( HasToBeProvided tag (ColumnList be s ('Tagged tag' a ': as)),
    ObtainColumn' be s (ColumnList be s as) tag typ
  ) =>
  ObtainColumn' be s (ColumnList be s ('Tagged tag' a ': as)) tag typ
  where
  obtainColumn' (TaggedCol _c `ColCons` as) = obtainColumn' as

instance
  {-# OVERLAPPING #-}
  (HasToBeProvided tag (ColumnList be s ('Tagged tag a ': as))) =>
  ObtainColumn' be s (ColumnList be s ('Tagged tag a ': as)) tag a
  where
  obtainColumn' (TaggedCol c `ColCons` _as) _ _ = c

class ObtainColumn be s a (tag :: Symbol) | a tag -> be s where
  obtainColumn :: a -> Proxy tag -> QExpr be s Void

instance
  {-# OVERLAPPABLE #-}
  forall be s a as (tag :: Symbol) (tag' :: Symbol).
  ( HasToBeProvided tag (ColumnList be s ('Tagged tag' a ': as)),
    ObtainColumn be s (ColumnList be s as) tag
  ) =>
  ObtainColumn be s (ColumnList be s ('Tagged tag' a ': as)) tag
  where
  obtainColumn (TaggedCol _c `ColCons` as) = obtainColumn as

instance
  {-# OVERLAPPING #-}
  (HasToBeProvided tag (ColumnList be s ('Tagged tag a ': as))) =>
  ObtainColumn be s (ColumnList be s ('Tagged tag a ': as)) tag
  where
  obtainColumn (TaggedCol c `ColCons` _as) _ =
    (coerce :: QExpr be s a -> QExpr be s Void) c
