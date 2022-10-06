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
import Servant
import Servant.Server.Internal.Context
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router
import qualified Text.Parsec as Parsec

type Offset = QueryParam "offset" Integer

type Limit = QueryParam "limit" Integer

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
      api = Proxy :: Proxy (QueryParam "offset" Integer :> QueryParam "limit" Integer :> api)

withDefaultPagination :: Pagination -> (Pagination -> a) -> (Maybe Integer -> Maybe Integer -> a)
withDefaultPagination defaultPagination f mOffset mLimit =
  f $
    Pagination
      { offset = fromMaybe (offset defaultPagination) mOffset,
        limit = fromMaybe (limit defaultPagination) mLimit,
      }
