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

module API.Modifiers.Internal where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Extended as T
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
