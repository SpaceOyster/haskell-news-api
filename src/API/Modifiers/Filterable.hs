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
  ( FilterableBy,
    FilterableBy',
    Tagged (..),
    Filter (..),
    Predicate (..),
    (:?),
  )
where

import API.Modifiers.Internal.PolyKinds
  ( Foldr,
    HasToBeInList,
    ReifySymbolsList (..),
    Replicate,
  )
import API.Modifiers.Internal.Tagged
  ( Tagged (..),
    (:?),
  )
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Kind (Type)
import qualified Data.Text.Extended as T
import GHC.Base (Constraint, Symbol)
import GHC.TypeLits (AppendSymbol, KnownSymbol)
import Servant
  ( Context,
    ErrorFormatters,
    FromHttpApiData (..),
    HasContextEntry,
    HasServer (..),
    Proxy (..),
    QueryParam (..),
    Server,
    type (:>),
  )
import Servant.Server.Internal.Delayed (Delayed)
import Servant.Server.Internal.ErrorFormatter
  ( MkContextWithErrorFormatter,
  )
import Servant.Server.Internal.Router (Router)
import qualified Text.Parsec as Parsec

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

predicateParser :: Parsec.Parsec T.Text st Predicate
predicateParser =
  Parsec.choice
    [ Parsec.string "eq" >> pure Equals,
      Parsec.string "lt" >> pure LessThan,
      Parsec.string "gt" >> pure GreaterThan,
      Parsec.string "neq" >> pure (Not Equals),
      (Parsec.string "nlt" <|> Parsec.string "gte") >> pure (Not LessThan),
      (Parsec.string "ngt" <|> Parsec.string "lte") >> pure (Not GreaterThan)
    ]


data Filter (tag :: Symbol) a = Filter
  { getPredicate :: Predicate,
    getValue :: a
  }


data FilterableBy' (tag :: Symbol) a


type family GeneratedTagsAreKnownSymbol (tag :: Symbol) :: Constraint where
  GeneratedTagsAreKnownSymbol tag =
    ( KnownSymbol (AppendSymbol tag "_eq"),
      KnownSymbol (AppendSymbol tag "_lt"),
      KnownSymbol (AppendSymbol tag "_gt"),
      KnownSymbol (AppendSymbol tag "_neq"),
      KnownSymbol (AppendSymbol tag "_nlt"),
      KnownSymbol (AppendSymbol tag "_ngt"),
      KnownSymbol (AppendSymbol tag "_gte"),
      KnownSymbol (AppendSymbol tag "_lte")
    ) ::
      Constraint

type family GenerateFilterQueryParams (filterName :: Symbol) typ where
  GenerateFilterQueryParams filterName typ =
    '[ QueryParam (AppendSymbol filterName "_eq") typ,
       QueryParam (AppendSymbol filterName "_lt") typ,
       QueryParam (AppendSymbol filterName "_gt") typ,
       QueryParam (AppendSymbol filterName "_neq") typ,
       QueryParam (AppendSymbol filterName "_nlt") typ,
       QueryParam (AppendSymbol filterName "_ngt") typ,
       QueryParam (AppendSymbol filterName "_gte") typ,
       QueryParam (AppendSymbol filterName "_lte") typ
     ]

type family GenerateFilterAPIType (filterName :: Symbol) typ api where
  GenerateFilterAPIType filterName typ api =
    Foldr (:>) api (GenerateFilterQueryParams filterName typ)
instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    Ord a,
    KnownSymbol tag,
    GeneratedTagsAreKnownSymbol tag,
    FromHttpApiData a
  ) =>
  HasServer (FilterableBy' tag a :> api) context
  where
  type ServerT (FilterableBy' tag a :> api) m = [Filter tag a] -> ServerT api m

  hoistServerWithContext ::
    Proxy (FilterableBy' tag a :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (FilterableBy' tag a :> api) m ->
    ServerT (FilterableBy' tag a :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    Proxy (FilterableBy' tag a :> api) ->
    Context context ->
    Delayed env (Server (FilterableBy' tag a :> api)) ->
    Router env
  route Proxy context delayed =
    route api context (withQParam <$> delayed)
    where
      api = Proxy :: Proxy (GenerateFilterAPIType tag a api)
      withQParam ::
        ([Filter tag a] -> x) ->
        ( Maybe a ->
          Maybe a ->
          Maybe a ->
          Maybe a ->
          Maybe a ->
          Maybe a ->
          Maybe a ->
          Maybe a ->
          x
        )
      withQParam f mEq mLt mGt mNeq mNlt mNgt mGte mLte =
        f $
          toList
            =<< [ Filter Equals <$> mEq,
                  Filter LessThan <$> mLt,
                  Filter GreaterThan <$> mGt,
                  Filter (Not Equals) <$> mNeq,
                  Filter (Not LessThan) <$> mNlt,
                  Filter (Not GreaterThan) <$> mNgt,
                  Filter (Not LessThan) <$> mGte,
                  Filter (Not GreaterThan) <$> mLte
                ]
