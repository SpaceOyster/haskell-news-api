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

import App.Config as Config
import Data.Maybe
import Data.Typeable
import Effects.Config
import Servant
import Servant.Server.Internal.Context
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router

type Offset = QueryParam "offset" Integer

type Limit = QueryParam "limit" Integer

data Paginated deriving (Typeable)

instance
  ( HasServer api context,
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
  route Proxy context delayed = route api context (withPagination <$> delayed)
    where
      api = Proxy :: Proxy (QueryParam "offset" Integer :> QueryParam "limit" Integer :> api)

getPagination :: MonadConfig m => Maybe Integer -> Maybe Integer -> m Pagination
getPagination mOffset mLimit = do
  pagination <- pagination . apiConfig <$> getConfig
  let offset = fromMaybe (Config.offset pagination) mOffset
      limit = fromMaybe (Config.limit pagination) mLimit
  pure $ Pagination {offset, limit}

withPagination :: (Pagination -> a) -> (Maybe Integer -> Maybe Integer -> a)
withPagination f mOffset mLimit =
  let offset = fromMaybe 0 mOffset
      limit = fromMaybe 25 mLimit
      p = Pagination {offset, limit}
   in f p
