{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Beam.Filterable where

import API.Modifiers.Beam.Internal
import API.Modifiers.Filterable
import API.Modifiers.Internal
import Data.Type.Bool (If)
import Data.Typeable (Proxy (..))
import Database.Beam.Backend.SQL
import Database.Beam.Query
import GHC.Base (Constraint)
import GHC.TypeLits

predicateToSql ::
  (SqlOrd expr a, SqlEq expr a) =>
  Predicate ->
  (a -> a -> expr Bool)
predicateToSql predicate =
  case predicate of
    Equals -> (==.)
    LessThan -> (<.)
    GreaterThan -> (>.)
    Not Equals -> (/=.)
    Not LessThan -> (>=.)
    Not GreaterThan -> (<=.)
    Not (Not p) -> predicateToSql p

newtype FilteringApp be s filterspec = FilteringApp
  { unFilteringApp :: ColumnList be s filterspec
  }

filterFor_ :: forall tag be s a. QExpr be s a -> TaggedColumn tag be s a
filterFor_ = TaggedCol

type BeamBackendSupportsValueSyntaxFor be typ =
  HasSqlValueSyntax
    ( Sql92ExpressionValueSyntax
        ( Sql92SelectTableExpressionSyntax
            ( Sql92SelectSelectTableSyntax
                ( Sql92SelectSyntax
                    (BeamSqlBackendSyntax be)
                )
            )
        )
    )
    typ

composeBeamFilter ::
  forall be s s' tag typ filterspec.
  ( BeamSqlBackend be,
    ObtainColumn' be s' (ColumnList be s filterspec) tag typ,
    HasSqlEqualityCheck be typ,
    BeamBackendSupportsValueSyntaxFor be typ
  ) =>
  FilteringApp be s filterspec ->
  Filter tag typ ->
  QExpr be s' Bool
composeBeamFilter (FilteringApp colList) (Filter predicate a) =
  column `predicateSQL` as_ @typ (val_ a)
  where
    predicateSQL = predicateToSql predicate
    column = obtainColumn' colList (Proxy @tag) (Proxy @typ)

filterBy_ ::
  ( BeamSqlBackend be,
    Projectible be a,
    ObtainColumn' be s' (ColumnList be s filterspec) tag typ,
    HasSqlEqualityCheck be typ,
    BeamBackendSupportsValueSyntaxFor be typ
  ) =>
  Filter tag typ ->
  (a -> FilteringApp be s filterspec) ->
  Q be db s' a ->
  Q be db s' a
filterBy_ freq filterApp = filter_ $ \a -> composeBeamFilter (filterApp a) freq

filterByMaybe_ ::
  ( BeamSqlBackend be,
    Projectible be a,
    ObtainColumn' be s' (ColumnList be s filterspec) tag typ,
    HasSqlEqualityCheck be typ,
    BeamBackendSupportsValueSyntaxFor be typ
  ) =>
  Maybe (Filter tag typ) ->
  (a -> FilteringApp be s filterspec) ->
  Q be db s' a ->
  Q be db s' a
filterByMaybe_ Nothing _filterApp = filter_ $ \_ -> val_ True
filterByMaybe_ (Just freq) filterApp = filterBy_ freq filterApp

filterByList_ ::
  forall a tag typ be db s s' filterspec.
  ( BeamSqlBackend be,
    Projectible be a,
    ObtainColumn' be s' (ColumnList be s filterspec) tag typ,
    HasSqlEqualityCheck be typ,
    BeamBackendSupportsValueSyntaxFor be typ
  ) =>
  [Filter tag typ] ->
  (a -> FilteringApp be s filterspec) ->
  Q be db s' a ->
  Q be db s' a
filterByList_ _filters@[] _filterApp = id
filterByList_ filters@(_ : _) filterApp =
  filter_ $ \table -> foldr1 (&&.) $ fmap (go table) filters
  where
    go :: a -> Filter tag typ -> QExpr be s' Bool
    go table = composeBeamFilter (filterApp table)

type family ValidFilteringApp req app :: Constraint where
  ValidFilteringApp (FilteringRequest filterTypes) (FilteringApp be s filterspec) =
    If
      (IsSubset (ListOfTags filterTypes) (ListOfTags filterspec))
      (() :: Constraint)
      ( TypeError
          ( 'Text "The '"
              ':<>: 'ShowType (FilteringApp be s filterspec)
              ':<>: 'Text "'"
              ':$$: 'Text "is not a valid handler for '"
                ':<>: 'ShowType (FilteringRequest filterTypes)
                ':<>: 'Text "' type"
          )
      )

class FilteringRequestBeam be s s' filterspec req where
  filterByRequest_ ::
    ( BeamSqlBackend be,
      Projectible be a
    ) =>
    req ->
    (a -> FilteringApp be s filterspec) ->
    Q be db s' a ->
    Q be db s' a

instance FilteringRequestBeam be s s' filterspec (FilteringRequest '[]) where
  filterByRequest_ _ _ = id

instance
  ( KnownSymbol tag,
    ObtainColumn' be s' (ColumnList be s filterspec) tag typ,
    HasSqlEqualityCheck be typ,
    BeamBackendSupportsValueSyntaxFor be typ,
    FilteringRequestBeam be s s' filterspec (FilteringRequest as),
    ValidFilteringApp (FilteringRequest ('Tagged tag typ ': as)) (FilteringApp be s filterspec)
  ) =>
  FilteringRequestBeam be s s' filterspec (FilteringRequest ('Tagged tag typ ': as))
  where
  filterByRequest_ (filters `FiltReqCons` fs) filterApp =
    filterByList_ filters filterApp . filterByRequest_ fs filterApp
