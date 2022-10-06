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

import Data.Maybe
import Data.Typeable
import Servant
import Servant.Server.Internal.Context
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router

type Offset = QueryParam "offset" Integer

type Limit = QueryParam "limit" Integer

data Pagination = Pagination
  { offset :: Integer,
    limit :: Integer,
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
  let offset = fromMaybe (Config.offset defaultPagination) mOffset
      limit = fromMaybe (Config.limit defaultPagination) mLimit
      p = Pagination {offset, limit}
   in f p
