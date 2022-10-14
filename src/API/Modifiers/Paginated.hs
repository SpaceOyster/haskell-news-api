{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Paginated where

import Data.Bifunctor (first)
import qualified Data.Configurator.Types as Conf
import Data.Foldable (asum)
import Data.Maybe
import Data.Text.Extended as T
import Data.Typeable
import Database.Beam (asc_, desc_)
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Query (QExpr)
import Database.Beam.Query.Internal (QOrd)
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

parseOrder :: Text -> Either Text Order
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

paginationOrder_ ::
  BeamSqlBackend be =>
  Pagination ->
  (QExpr be s a -> QOrd be s a)
paginationOrder_ p = case order p of
  Asc -> asc_
  Desc -> desc_

data Pagination = Pagination
  { offset :: Integer,
    limit :: Integer,
    order :: Order
  }
  deriving (Eq, Show)

data Paginated deriving (Typeable)

instance
  ( HasServer api context,
    HasContextEntry context Pagination,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (Paginated :> api) context
  where
  type ServerT (Paginated :> api) m = Pagination -> ServerT api m

  hoistServerWithContext ::
    Proxy (Paginated :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (Paginated :> api) m ->
    ServerT (Paginated :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    Proxy (Paginated :> api) ->
    Context context ->
    Delayed env (Server (Paginated :> api)) ->
    Router env
  route Proxy context delayed =
    route api context (withDefaultPagination defaultPagination <$> delayed)
    where
      defaultPagination = getContextEntry context :: Pagination
      api = Proxy :: Proxy (QueryParam "offset" Integer :> QueryParam "limit" Integer :> QueryParam "order" Order :> api)

withDefaultPagination :: Pagination -> (Pagination -> a) -> (Maybe Integer -> Maybe Integer -> Maybe Order -> a)
withDefaultPagination defaultPagination f mOffset mLimit mOrder =
  f $
    Pagination
      { offset = fromMaybe (offset defaultPagination) mOffset,
        limit = fromMaybe (limit defaultPagination) mLimit,
        order = fromMaybe (order defaultPagination) mOrder
      }
