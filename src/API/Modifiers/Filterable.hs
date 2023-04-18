{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Filterable
  ( FilterableBy,
    FilterableBy',
    Tagged (..),
    Filter (..),
    Predicate (..),
  )
where

import API.Modifiers.Internal.PolyKinds
  ( ConcatConstraints,
    Fmap,
    Foldr,
    HasToBeInList,
    ReifySymbolsList (..),
    Replicate,
  )
import API.Modifiers.Internal.Tagged
  ( Tagged (..),
    (:?),
  )
import Control.Applicative ((<|>))
import Data.Data
import Data.Foldable (toList)
import Data.Kind (Type)
import qualified Data.Text.Extended as T
import GHC.Base
import GHC.Generics
import GHC.TypeLits
import Servant
  ( Context,
    ErrorFormatters,
    FromHttpApiData (..),
    HasContextEntry,
    HasServer (..),
    Proxy (..),
    QueryParam (..),
    Server,
    type (:<|>),
    type (:>),
  )
import Servant.Server.Internal.Delayed (Delayed)
import Servant.Server.Internal.ErrorFormatter
  ( MkContextWithErrorFormatter,
  )
import Servant.Server.Internal.Router (Router)
import qualified Text.Parsec as Parsec

data FilterableBy (a :: [Tagged Type])


type family ValidFilterSpec a :: Constraint where
  ValidFilterSpec '[] = () :: Constraint
  ValidFilterSpec (Tagged s t ': as) = ValidFilterSpec as
  ValidFilterSpec (a ': as) =
    TypeError
      ( 'Text "Filter spec list item '"
          ':<>: 'ShowType a
          ':<>: 'Text "' is illegal."
          ':$$: 'Text "Filter spec list should contain only Data.Tagged.Tagged types."
      )
instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (FilterableBy '[] :> api) context
  where
  type ServerT (FilterableBy '[] :> api) m = ServerT api m
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)
  route Proxy = route (Proxy @api)

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    KnownSymbol ta,
    GeneratedTagsAreKnownSymbol ta,
    HasServer (FilterableBy' ta a :> api) context,
    HasServer (FilterableBy as :> api) context,
    FromHttpApiData a,
    Ord a
  ) =>
  HasServer (FilterableBy ('Tagged ta a ': as) :> api) context
  where
  type
    ServerT (FilterableBy ('Tagged ta a ': as) :> api) m =
      [Filter ta a] -> ServerT (FilterableBy as :> api) m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy (FilterableBy as :> api)) pc nt . s

  route Proxy = route api
    where
      api = Proxy :: Proxy (FilterableBy' ta a :> FilterableBy as :> api)

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

-- | Used in generating allowed query params for filters.
-- >>> ../user?registration-date_lt="01-01-2001"
type PredicateSymbols :: [Symbol]
type PredicateSymbols = ["eq", "lt", "gt", "neq", "nlt", "ngt", "gte", "lte"]

-- | Generates list of Symbols by prepending specified tag to each Symbol of
-- provided list, separating them with "_" symbol.
-- >>> :k! PrependTagToSuffixes "name" ["1", "2"]
-- PrependTagToSuffixes "name" ["1", "2"] :: [Symbol]
-- = '["name_1", "name_2"]
type family PrependTagToSuffixes (tag :: Symbol) (suffixes :: [Symbol]) :: [Symbol] where
  PrependTagToSuffixes tag '[] = '[]
  PrependTagToSuffixes tag (a ': as) =
    AppendSymbol tag (AppendSymbol "_" a) ': PrependTagToSuffixes tag as

-- | Generates list of query param names for a 'filter' by prependig it's name
-- to each of PredicateSymbols sepparating them with "_" symbol.
-- >>> :k! GenerateFilterTags "filter"
-- GenerateFilterTags "filter" :: [Symbol]
-- = '["filter_eq", "filter_lt", "filter_gt", "filter_neq",
--     "filter_nlt", "filter_ngt", "filter_gte", "filter_lte"]
type family GenerateFilterTags (tag :: Symbol) :: [Symbol] where
  GenerateFilterTags tag = PrependTagToSuffixes tag PredicateSymbols

-- | Generates a list of 'KnownSymbol' Constraint for each Symbol
-- of 'GenerateFilterTags tag' list and concatenates them.
-- >>> :k! GeneratedTagsAreKnownSymbol "filter"
-- GeneratedTagsAreKnownSymbol "filter" :: Constraint
-- = (KnownSymbol "filter_eq",
--    (KnownSymbol "filter_lt",
--    ...
--         (KnownSymbol "filter_gte",
--          (KnownSymbol "filter_lte", () :: Constraint))))))))
type family GeneratedTagsAreKnownSymbol (tag :: Symbol) :: Constraint where
  GeneratedTagsAreKnownSymbol tag =
    ConcatConstraints (Fmap KnownSymbol (GenerateFilterTags tag))

-- | Generates a list of 'QueryParam' for list of Symbols and specified type.
-- >>> :k! ApplyQueryParam ["one", "two"] Integer
-- ApplyQueryParam ["one", "two"] Integer :: [*]
-- = '[QueryParam "one" Integer, QueryParam "two" Integer]
type family ApplyQueryParam (filterTags :: [Symbol]) typ where
  ApplyQueryParam '[] typ = '[]
  ApplyQueryParam (a ': as) typ =
    QueryParam a typ ': ApplyQueryParam as typ

-- | Generates a list of 'QueryParam' for specified 'filterName' and type.
-- >>> :k! GenerateFilterQueryParams "created-at" UTCTime
-- GenerateFilterQueryParams "created-at" UTCTime :: [*]
-- = '[QueryParam "created-at_eq" UTCTime,
--     QueryParam "created-at_lt" UTCTime,
--     ...
--     QueryParam "created-at_gte" UTCTime,
--     QueryParam "created-at_lte" UTCTime]
type family GenerateFilterQueryParams (filterName :: Symbol) typ where
  GenerateFilterQueryParams filterName typ =
    ApplyQueryParam (GenerateFilterTags filterName) typ

-- | Generates 'Servant' API type by generated a list of 'QueryParam',
-- inteerspersing them with '(:>)' operator and appending ':> api' at the end.
-- >>> :k! GenerateFilterAPIType "registered-at" UTCTime (Get '[JSON] User)
-- GenerateFilterAPIType "registered-at" UTCTime (Get '[JSON] User) :: *
-- = QueryParam' '[Optional, Strict] "registered-at_eq" UTCTime
--   :> (QueryParam' '[Optional, Strict] "registered-at_lt" UTCTime
--       :> (QueryParam' '[Optional, Strict] "registered-at_gt" UTCTime
--       ...
--         :> (QueryParam' '[Optional, Strict] "registered-at_lte" UTCTime
--           :> Verb 'GET 200 '[JSON] User)))))))
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
