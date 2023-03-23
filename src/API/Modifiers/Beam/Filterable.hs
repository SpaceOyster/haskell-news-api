{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module API.Modifiers.Beam.Filterable where

import API.Modifiers.Beam.Internal
import API.Modifiers.Filterable
import API.Modifiers.Internal.PolyKinds
import API.Modifiers.Internal.Tagged
import Data.Typeable (Proxy (..))
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.AST
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Query
import Database.Beam.Query.Internal

predicateToSql ::
  (SqlOrd expr a, SqlEq expr a) =>
  Predicate ->
  (a -> a -> expr Bool)
predicateToSql pred =
  case pred of
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
composeBeamFilter (FilteringApp colList) (Filter pred a) =
  column `predicate` as_ @typ (val_ a)
  where
    predicate = predicateToSql pred
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

filterByMaybe_ Nothing filterApp = filter_ $ \_ -> val_ True
filterByMaybe_ (Just freq) filterApp = filterBy_ freq filterApp

-- INFO: `. filter_ (\u -> val_ True)` === unfiltered query
