{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Sortable
  ( sortBy_,
    (.:.),
    ColumnList (ColNil),
    sorterFor_,
    Sorting (Ascend, Descend),
    SortableBy,
    SortingApp (SortingApp, unSortingApp),
    SortingRequest (SortingRequest, unSortingRequest),
  )
where

import API.Modifiers.Internal
  ( ColumnList (ColNil),
    HasToBeInList,
    LookupColumn (lookupColumn),
    TaggedColumn (TaggedCol),
    ValidNamesList (),
    symbolCIText,
    (.:.),
  )
import qualified API.Modifiers.Internal as Internal
import Data.Bifunctor (first)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (asum)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text.Extended as T
import Data.Typeable
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
import GHC.Base
import GHC.TypeLits
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router
import qualified Text.Parsec as Parsec

data Sorting a = Ascend a | Descend a
  deriving (Eq, Show)

unSorting :: Sorting a -> a
unSorting (Ascend a) = a
unSorting (Descend a) = a

sortingParser :: Parsec.Parsec String st (a -> Sorting a)
sortingParser =
  asum
    [ Parsec.string "asc" >> pure Ascend,
      Parsec.string "desc" >> pure Descend
    ]

parseSorting :: T.Text -> Either T.Text (a -> Sorting a)
parseSorting =
  first T.tshow
    . Parsec.parse sortingParser "Pagination Order"
    . T.unpack
    . T.toLower

instance FromHttpApiData (a -> Sorting a) where
  parseUrlPiece = parseSorting

class ReifySorting (sorting :: Sorting Symbol) where
  reifySorting :: Sorting (CI T.Text)

instance (KnownSymbol a) => ReifySorting ('Ascend a) where
  reifySorting = Ascend $ symbolCIText $ Proxy @a

instance (KnownSymbol a) => ReifySorting ('Descend a) where
  reifySorting = Descend $ symbolCIText $ Proxy @a

type family ExtractColumnNameFromSorting (sorting :: Sorting Symbol) where
  ExtractColumnNameFromSorting ('Ascend a) = a
  ExtractColumnNameFromSorting ('Descend a) = a

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

newtype SortingRequest (available :: [Symbol]) = SortingRequest
  { unSortingRequest :: Sorting (CI T.Text)
  }

sortBy_ ::
  ( BeamSqlBackend be,
    BeamSqlBackend be',
    Projectible be a,
    SqlOrderable be (QOrd be' s' Void),
    ThreadRewritable (QNested s) a,
    LookupColumn be (QNested s) (ColumnList be (QNested s) sortspec)
  ) =>
  Sorting (CI T.Text) ->
  (a -> SortingApp be (QNested s) sortspec) ->
  Q be db (QNested s) a ->
  Q be db s (WithRewrittenThread (QNested s) s a)
sortBy_ sorting sortApp = orderBy_ $ \a ->
  let colList = unSortingApp $ sortApp a
      colName = unSorting sorting
   in sortingOrder_ sorting (fromJust $ lookupColumn colList colName)

type DefaultColumnName (deflt :: Sorting Symbol) =
  ExtractColumnNameFromSorting deflt

type SortingSpec (available :: [Symbol]) (deflt :: Sorting Symbol) =
  ( HasToBeInList (DefaultColumnName deflt) available,
    ReifySorting deflt,
    ValidNamesList available
  ) ::
    Constraint

validateSortingName ::
  forall available deflt.
  SortingSpec (available :: [Symbol]) (deflt :: Sorting Symbol) =>
  (CI T.Text -> Sorting (CI T.Text)) ->
  CI T.Text ->
  Sorting (CI T.Text)
validateSortingName ordering name =
  if Internal.isNameValid @available name
    then ordering name
    else reifySorting @deflt

data SortableBy (available :: [Symbol]) (deflt :: Sorting Symbol)

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    SortingSpec available deflt,
    ReifySorting deflt
  ) =>
  HasServer (SortableBy available deflt :> api) context
  where
  type ServerT (SortableBy available deflt :> api) m = Sorting (CI T.Text) -> ServerT api m

  hoistServerWithContext ::
    Proxy (SortableBy available deflt :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (SortableBy available deflt :> api) m ->
    ServerT (SortableBy available deflt :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    ( SortingSpec available deflt,
      ReifySorting deflt
    ) =>
    Proxy (SortableBy available deflt :> api) ->
    Context context ->
    Delayed env (Server (SortableBy available deflt :> api)) ->
    Router env
  route Proxy context delayed =
    route api context (provideSorting <$> delayed)
    where
      api = Proxy :: Proxy (QueryParam "order" (a -> Sorting a) :> QueryParam "sort-by" T.Text :> api)
      defaultSorting = reifySorting @deflt
      provideSorting f mOrder mSortBy =
        let ordering = fromMaybe Ascend mOrder
            sortField = fromMaybe (T.pack "") mSortBy
         in f . validateSortingName @available @deflt ordering $ CI.mk sortField
