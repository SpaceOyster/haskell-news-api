{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Paginated where

import qualified Data.Configurator.Types as Conf
import Data.Maybe
import Data.Typeable
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router

data Pagination = Pagination
  { offset :: Integer,
    limit :: Integer
  }
  deriving (Eq, Show)

data Paginated deriving (Typeable)

data PaginationConfig = PaginationConfig
  { defaultOffset :: Integer,
    defaultLimit :: Integer,
    maxLimit :: Integer
  }
  deriving (Show)

instance
  ( HasServer api context,
    HasContextEntry context PaginationConfig,
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
    route api context (withPaginationConfig defaultPagination <$> delayed)
    where
      defaultPagination = getContextEntry context :: PaginationConfig
      api = Proxy :: Proxy (QueryParam "offset" Integer :> QueryParam "limit" Integer :> api)

withPaginationConfig :: PaginationConfig -> (Pagination -> a) -> (Maybe Integer -> Maybe Integer -> a)
withPaginationConfig pConfig f mOffset mLimit =
  let offset = fromMaybe (defaultOffset pConfig) mOffset
      limit = min (maxLimit pConfig) (fromMaybe (defaultLimit pConfig) mLimit)
   in f $ Pagination {offset, limit}
