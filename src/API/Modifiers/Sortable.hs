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
import qualified Data.Configurator.Types as Conf
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import Data.Typeable
import Database.Beam (asc_, desc_)
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Query (QExpr)
import Database.Beam.Query.Internal (QOrd)
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

data SortingParams = SortingParams {order :: Order}

sortingOrder_ ::
  BeamSqlBackend be =>
  SortingParams ->
  (QExpr be s a -> QOrd be s a)
sortingOrder_ p = case order p of
  Asc -> asc_
  Desc -> desc_

data Sorted deriving (Typeable)

data SortedBy (available :: [Symbol]) (deflt :: Symbol)

symbolCIText :: (KnownSymbol a) => Proxy a -> CI T.Text
symbolCIText = CI.mk . T.pack . symbolVal

class LookupNameWithDefault (availbale :: [Symbol]) (deflt :: Symbol) where
  lookupName :: CI T.Text -> CI T.Text

instance KnownSymbol deflt => LookupNameWithDefault '[] deflt where
  lookupName _ = symbolCIText $ Proxy @deflt

instance
  (KnownSymbol a, LookupNameWithDefault as deflt) =>
  LookupNameWithDefault (a : as) deflt
  where
  lookupName t
    | symbolCIText (Proxy @a) == t = t
    | otherwise = lookupName @as @deflt t

instance
  ( HasServer api context,
    HasContextEntry context SortingParams,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (Sorted :> api) context
  where
  type ServerT (Sorted :> api) m = SortingParams -> ServerT api m

  hoistServerWithContext ::
    Proxy (Sorted :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (Sorted :> api) m ->
    ServerT (Sorted :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    Proxy (Sorted :> api) ->
    Context context ->
    Delayed env (Server (Sorted :> api)) ->
    Router env
  route Proxy context delayed =
    route api context (withDefaultPagination defaultPagination <$> delayed)
    where
      defaultPagination = getContextEntry context :: SortingParams
      api = Proxy :: Proxy (QueryParam "order" Order :> api)

withDefaultPagination :: SortingParams -> (SortingParams -> a) -> (Maybe Order -> a)
withDefaultPagination defaultPagination f mOrder =
  f $
    SortingParams
      { order = fromMaybe (order defaultPagination) mOrder
      }
