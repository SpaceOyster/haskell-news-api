{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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

sortingOrder_ ::
  BeamSqlBackend be =>
  Sorting a' ->
  (QExpr be s a -> QOrd be s a)
sortingOrder_ (Ascend _) = asc_
sortingOrder_ (Descend _) = desc_

data TaggedColumn be s a = TaggedC (CI T.Text) (QExpr be s a)

taggedColumnToPair :: TaggedColumn be s a -> (CI T.Text, QExpr be s Void)
taggedColumnToPair (TaggedC t f) = (t, coerce f)

taggedColumnsToMap :: [TaggedColumn be s a] -> Map.Map (CI T.Text) (QExpr be s Void)
taggedColumnsToMap = Map.fromList . fmap taggedColumnToPair

sorterFor_ :: CI T.Text -> QExpr be s a -> TaggedColumn be s Void
sorterFor_ t = TaggedC t . coerce

sortBy_ ::
  ( BeamSqlBackend be,
    BeamSqlBackend be',
    Projectible be a,
    SqlOrderable be (QOrd be' s' Void),
    ThreadRewritable (QNested s) a
  ) =>
  Sorting (CI T.Text) ->
  (a -> [TaggedColumn be' s' Void]) ->
  Q be db (QNested s) a ->
  Q be db s (WithRewrittenThread (QNested s) s a)
sortBy_ sorting sorters = orderBy_ $ \a ->
  sortingOrder_ sorting (sortersMap a Map.! unSorting sorting)
  where
    sortersMap a = taggedColumnsToMap $ sorters a

class ReifySorting (sorting :: Sorting Symbol) where
  reifySorting :: Sorting (CI T.Text)

instance (KnownSymbol a) => ReifySorting ('Ascend a) where
  reifySorting = Ascend $ symbolCIText $ Proxy @a

instance (KnownSymbol a) => ReifySorting ('Descend a) where
  reifySorting = Descend $ symbolCIText $ Proxy @a

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

data SortableBy (available :: [Symbol]) (deflt :: Sorting Symbol)

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    LookupName available,
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
    ( LookupName available,
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
      provideSorting f mOrder mSortBy = f $ fromMaybe defaultSorting $ do
        ordering <- mOrder
        sortField <- mSortBy
        ordering <$> lookupName @available (CI.mk sortField)
