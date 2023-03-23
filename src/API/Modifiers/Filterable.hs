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

module API.Modifiers.Filterable
  ( FilterableBy (..),
    Tagged (..),
    (:?),
  )
where

import API.Modifiers.Internal.Tagged (Tagged (..), (:?))
import qualified Data.Text.Extended as T
import Servant
  ( Context,
    ErrorFormatters,
    FromHttpApiData (..),
    HasContextEntry,
    HasServer (..),
    Proxy (..),
    Server,
    type (:>),
  )
import Servant.Server.Internal.Delayed (Delayed)
import Servant.Server.Internal.ErrorFormatter
  ( MkContextWithErrorFormatter,
  )
import Servant.Server.Internal.Router (Router)
data FilterableBy (a :: [Tagged Type])

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (FilterableBy available :> api) context
  where
  type ServerT (FilterableBy available :> api) m = ServerT api m

  hoistServerWithContext ::
    Proxy (FilterableBy available :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (FilterableBy available :> api) m ->
    ServerT (FilterableBy available :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route ::
    Proxy (FilterableBy available :> api) ->
    Context context ->
    Delayed env (Server (FilterableBy available :> api)) ->
    Router env
  route Proxy context delayed = route api context delayed
    where
      api = Proxy :: Proxy api

data Predicate
  = Equals
  | LessThan
  | GreaterThan
  | Not Predicate
  deriving (Show, Eq, Ord)
