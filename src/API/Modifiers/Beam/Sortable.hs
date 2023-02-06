{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module API.Modifiers.Beam.Sortable where

import API.Modifiers.Internal
import API.Modifiers.Sortable
import Data.Maybe (fromJust)
import Data.Void
import Database.Beam (asc_, desc_, orderBy_)
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Query (QExpr, SqlOrderable)
import Database.Beam.Query.Internal
  ( Projectible,
    Q,
    QNested,
    QOrd,
    ThreadRewritable,
    WithRewrittenThread,
  )

sortingOrder_ ::
  BeamSqlBackend be =>
  Sorting a' ->
  (QExpr be s a -> QOrd be s a)
sortingOrder_ (Ascend _) = asc_
sortingOrder_ (Descend _) = desc_

sorterFor_ :: forall tag be s a. QExpr be s a -> TaggedColumn tag be s a
sorterFor_ = TaggedCol

newtype SortingApp be s sortspec = SortingApp
  { unSortingApp :: ColumnList be s sortspec
  }

sortBy_ ::
  ( BeamSqlBackend be,
    BeamSqlBackend be',
    Projectible be a,
    SqlOrderable be (QOrd be' s' Void),
    ThreadRewritable (QNested s) a,
    LookupColumn be (QNested s) (ColumnList be (QNested s) sortspec),
    HasToBeSubset available (ListOfTags sortspec)
  ) =>
  SortingRequest available ->
  (a -> SortingApp be (QNested s) sortspec) ->
  Q be db (QNested s) a ->
  Q be db s (WithRewrittenThread (QNested s) s a)
sortBy_ (SortingRequest sorting) sortApp = orderBy_ $ \a ->
  let colList = unSortingApp $ sortApp a
      colName = unSorting sorting
   in sortingOrder_ sorting (fromJust $ lookupColumn colList colName)
