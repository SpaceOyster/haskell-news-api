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

import Data.Maybe
import Data.Typeable
import Servant
import Servant.Docs (HasDocs (docsFor))
import qualified Servant.Docs.Internal as Docs
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

instance (HasDocs api) => HasDocs (Paginated :> api) where
  docsFor Proxy (endpoint, action) = docsFor subAPI (endpoint, action')
    where
      subAPI = Proxy :: Proxy api
      action' = action {Docs._params = Docs._params action <> docQParam}
      docQParam =
        [ Docs.DocQueryParam
            { Docs._paramName = "offset",
              Docs._paramValues = ["Natural Number"],
              Docs._paramDesc = "Pagination offset. Skips first N items returned list. Default is 0.",
              Docs._paramKind = Docs.Normal
            },
          Docs.DocQueryParam
            { Docs._paramName = "limit",
              Docs._paramValues = ["Natural Number"],
              Docs._paramDesc = "Pagination limit. Limits the number of returned items. Default is specified in server config.",
              Docs._paramKind = Docs.Normal
            }
        ]
