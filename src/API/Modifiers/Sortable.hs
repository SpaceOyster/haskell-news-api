{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Sortable where

import Data.Bifunctor (first)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator.Types as Conf
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import Data.Typeable
import Data.Void
import Database.Beam (Columnar, asc_, desc_, orderBy_)
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Query (QExpr, SqlOrderable)
import Database.Beam.Query.Internal (Projectible, Q, QNested, QOrd, ThreadRewritable, WithRewrittenThread)
import GHC.Base
import GHC.TypeLits
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router
import qualified Text.Parsec as Parsec

data Order = Asc | Desc
  deriving (Eq, Show)

orderParser :: Parsec.Parsec String st Order
orderParser =
  asum
    [ Parsec.string "asc" >> pure Asc,
      Parsec.string "desc" >> pure Desc
    ]

parseOrder :: T.Text -> Either T.Text Order
parseOrder =
  first T.tshow
    . Parsec.parse orderParser "Pagination Order"
    . T.unpack
    . T.toLower

instance Conf.Configured Order where
  convert (Conf.String t) = either (const Nothing) pure $ parseOrder t
  convert _ = Nothing

instance FromHttpApiData Order where
  parseUrlPiece = parseOrder

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

sortingOrder' ::
  BeamSqlBackend be =>
  Sorting a' ->
  (QExpr be s a -> QOrd be s a)
sortingOrder' (Ascend _) = asc_
sortingOrder' (Descend _) = desc_

data SortingParams = SortingParams {order :: Order, sortBy :: CI T.Text}

sortingOrder_ ::
  BeamSqlBackend be =>
  SortingParams ->
  (QExpr be s a -> QOrd be s a)
sortingOrder_ p = case order p of
  Asc -> asc_
  Desc -> desc_


sorterFor :: CI T.Text -> QExpr be s a -> (CI T.Text, QExpr be s Void)
sorterFor name field = (name, coerce field)

sortBy_ ::
  ( BeamSqlBackend be,
    BeamSqlBackend be',
    Projectible be a,
    SqlOrderable be (QOrd be' s' Void),
    ThreadRewritable (QNested s) a
  ) =>
  SortingParams ->
  (a -> Map.Map (CI T.Text) (QExpr be' s' Void)) ->
  Q be db (QNested s) a ->
  Q be db s (WithRewrittenThread (QNested s) s a)
sortBy_ sorting sorters = orderBy_ (\a -> sortingOrder_ sorting $ sorters a Map.! sortBy sorting)

data SortableBy (available :: [Symbol]) (deflt :: Symbol)

symbolCIText :: (KnownSymbol a) => Proxy a -> CI T.Text
symbolCIText = CI.mk . T.pack . symbolVal

class LookupName (fieldNames :: [Symbol]) where
  lookupName :: CI T.Text -> Maybe (CI T.Text)

instance LookupName '[] where
  lookupName _ = Nothing

instance
  (KnownSymbol a, LookupName as) =>
  LookupName (a : as)
  where
  lookupName t
    | symbolCIText (Proxy @a) == t = Just t
    | otherwise = lookupName @as t

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    LookupName available,
    KnownSymbol deflt
  ) =>
  HasServer (SortableBy available deflt :> api) context
  where
  type ServerT (SortableBy available deflt :> api) m = SortingParams -> ServerT api m

  hoistServerWithContext ::
    Proxy (SortableBy available deflt :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (SortableBy available deflt :> api) m ->
    ServerT (SortableBy available deflt :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    ( KnownSymbol deflt,
      LookupName available
    ) =>
    Proxy (SortableBy available deflt :> api) ->
    Context context ->
    Delayed env (Server (SortableBy available deflt :> api)) ->
    Router env
  route Proxy context delayed =
    route api context (provideSortingParams <$> delayed)
    where
      api = Proxy :: Proxy (QueryParam "order" Order :> QueryParam "sort-by" T.Text :> api)
      defaultSorter =
        SortingParams
          { order = Asc,
            sortBy = symbolCIText (Proxy @deflt)
          }
      provideSortingParams f mOrder mSortBy =
        f $ fromMaybe defaultSorter $ do
          sortBy <- lookupName @available $ CI.mk $ fromMaybe mempty mSortBy
          pure $ SortingParams {order = fromMaybe Asc mOrder, sortBy}
