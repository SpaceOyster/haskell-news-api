{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Beam.Sortable
  ( SortingApp (..),
    sorterFor_,
    sortingOrder_,
    composeBeamOrdering,
    sortBy_,
    ColumnList (..),
    (.:.),
  )
where

import API.Modifiers.Beam.Internal
  ( ColumnList (..),
    LookupColumn (lookupColumn),
    ObtainColumn (obtainColumn),
    TaggedColumn (TaggedCol),
    (.:.),
  )
import API.Modifiers.Internal
import API.Modifiers.Sortable
import Data.Maybe (fromMaybe)
import Data.Type.Bool (If, type (&&))
import Data.Typeable (Proxy (..))
import Data.Void
import Database.Beam (asc_, desc_, orderBy_)
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Query (QExpr)
import Database.Beam.Query.Internal
  ( Projectible,
    Q,
    QNested,
    QOrd,
    ThreadRewritable,
    WithRewrittenThread,
  )
import GHC.Base (Constraint)
import GHC.TypeLits.Extended
  ( ErrorMessage (ShowType, Text, (:$$:), (:<>:)),
    TypeError,
  )

sortingOrder_ ::
  (BeamSqlBackend be) =>
  Sorting a' ->
  (QExpr be s a -> QOrd be s a)
sortingOrder_ (Ascend _) = asc_
sortingOrder_ (Descend _) = desc_

sorterFor_ :: forall tag be s a. QExpr be s a -> TaggedColumn tag be s a
sorterFor_ = TaggedCol

newtype SortingApp be s sortspec = SortingApp
  { unSortingApp :: ColumnList be s sortspec
  }

composeBeamOrdering ::
  forall be be' s s' sortspec available deflt.
  ( BeamSqlBackend be,
    BeamSqlBackend be',
    ReifySorting deflt,
    LookupColumn be' s' (ColumnList be s sortspec),
    ObtainColumn be' s' (ColumnList be s sortspec) (UnSorting deflt)
  ) =>
  SortingApp be s sortspec ->
  SortingRequest available deflt ->
  QOrd be' s' Void
composeBeamOrdering (SortingApp colList) sreq =
  fromMaybe (defaultOrder defaultColumn) mOrd
  where
    sorting = unSortingRequest sreq
    colName = unSorting sorting
    sortingOrder = sortingOrder_ sorting
    mColumn = lookupColumn colList colName
    mOrd = sortingOrder <$> mColumn
    defaultSorting = reifySorting @deflt
    defaultOrder = sortingOrder_ defaultSorting
    defaultColumn = obtainColumn colList (Proxy :: Proxy (UnSorting deflt))

type family ValidSortingApp req app :: Constraint where
  ValidSortingApp (SortingRequest available deflt) (SortingApp be s sortspec) =
    If
      ( IsSubset available (ListOfTags sortspec)
          && SortingIsAvailable deflt available
      )
      (() :: Constraint)
      ( TypeError
          ( 'Text "The '"
              ':<>: 'ShowType (SortingApp be s sortspec)
              ':<>: 'Text "'"
              ':$$: 'Text "is not a valid handler for '"
              ':<>: 'ShowType (SortingRequest available deflt)
              ':<>: 'Text "' type"
          )
      )

sortBy_ ::
  forall be s s' db a available deflt sortspec.
  ( s' ~ QNested s,
    BeamSqlBackend be,
    Projectible be a,
    ThreadRewritable s' a,
    ReifySorting deflt,
    ValidSortingApp (SortingRequest available deflt) (SortingApp be s sortspec),
    SortingHasToBeAvailable deflt available,
    LookupColumn be s' (ColumnList be s' sortspec),
    ObtainColumn be s' (ColumnList be s' sortspec) (UnSorting deflt)
  ) =>
  SortingRequest available deflt ->
  (a -> SortingApp be s' sortspec) ->
  Q be db s' a ->
  Q be db s (WithRewrittenThread s' s a)
sortBy_ sreq sortApp = orderBy_ $ \a -> composeBeamOrdering (sortApp a) sreq
