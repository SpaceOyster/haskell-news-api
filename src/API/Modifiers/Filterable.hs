{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
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
    FilterableBySingle,
    FilteringRequest (..),
    Tagged (..),
    Filter (..),
    Predicate (..),
  )
where

import API.Modifiers.Internal.Tagged
  ( Tagged (..),
  )
import Control.Applicative ((<|>))
import Data.Data
import Data.Kind (Type)
import qualified Data.Text.Extended as T
import qualified Data.Text.Lazy as T (fromStrict)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import GHC.TypeLits
import Network.HTTP.Types.URI (QueryText, queryToQueryText)
import Network.Wai (Request (queryString))
import Servant
  ( Context,
    ErrorFormatters,
    FromHttpApiData (..),
    HasContextEntry,
    HasServer (..),
    Server,
    ServerError (errBody),
    err400,
    type (:>),
  )
import Servant.Server.Internal.Delayed (Delayed, addParameterCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal, withRequest)
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
    HasServer (FilterableBySingle ta a :> api) context,
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
      api = Proxy :: Proxy (FilterableBySingle ta a :> FilterableBy as :> api)

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

class FilterValue a where
  parseFilterValue :: T.Text -> Either T.Text a

instance (FromHttpApiData a) => FilterValue a where
  parseFilterValue = parseQueryParam

filterQueryKeyParser ::
  forall tag a st.
  (KnownSymbol tag) =>
  Parsec.Parsec T.Text st (a -> Filter tag a)
filterQueryKeyParser = do
  let key = symbolVal (Proxy @tag)
  predicate <- Parsec.string key >> Parsec.char '_' >> predicateParser
  pure $ Filter predicate

doParseFilterQueryKey ::
  forall tag a.
  (KnownSymbol tag) =>
  T.Text ->
  Either T.Text (a -> Filter tag a)
doParseFilterQueryKey keyT =
  case Parsec.runParser (filterQueryKeyParser @tag @a) () (T.unpack keyT) keyT of
    Left _ -> Left gotWrongMessage
    Right p -> Right p
  where
    gotWrongMessage = T.pack msg
    expectedKey = symbolVal (Proxy @tag)
    msg =
      concat
        [ "Expected \"",
          expectedKey,
          "_<predicate>\" but got: ",
          show keyT,
          "."
        ]

parseQueryText ::
  forall tag a.
  (KnownSymbol tag, FilterValue a) =>
  QueryText ->
  Either T.Text [Filter tag a]
parseQueryText qt = sequence $ foldr go [] qt
  where
    gotEmptyValueFor key =
      T.pack $ concat ["Got QueryFlag, the value of ", show key, " is empty."]
    doParseValue :: (FilterValue a) => T.Text -> Maybe T.Text -> Either T.Text a
    doParseValue key Nothing = Left $ gotEmptyValueFor key
    doParseValue _ (Just val) = parseFilterValue val
    go :: (T.Text, Maybe T.Text) -> [Either T.Text (Filter tag a)] -> [Either T.Text (Filter tag a)]
    go (key, valM) b = case doParseFilterQueryKey key of
      Left _ -> b
      rPredicate -> (rPredicate <*> doParseValue key valM) : b

data Filter (tag :: Symbol) a = Filter
  { getPredicate :: Predicate,
    getValue :: a
  }

instance (Show a, KnownSymbol tag, Typeable a) => Show (Filter tag a) where
  show (Filter p v) =
    concat
      [ "(Filter { getPredicate = ",
        show p,
        ", getValue = ",
        show v,
        "} :: ",
        show $ typeRep $ Proxy @(Filter tag a),
        ")"
      ]

data FilterableBySingle (tag :: Symbol) a

type PredicateSymbols :: [Symbol]
type PredicateSymbols = ["eq", "lt", "gt", "neq", "nlt", "ngt", "gte", "lte"]

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    Ord a,
    KnownSymbol tag,
    FromHttpApiData a
  ) =>
  HasServer (FilterableBySingle tag a :> api) context
  where
  type ServerT (FilterableBySingle tag a :> api) m = [Filter tag a] -> ServerT api m

  hoistServerWithContext ::
    Proxy (FilterableBySingle tag a :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (FilterableBySingle tag a :> api) m ->
    ServerT (FilterableBySingle tag a :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    Proxy (FilterableBySingle tag a :> api) ->
    Context context ->
    Delayed env (Server (FilterableBySingle tag a :> api)) ->
    Router env
  route Proxy context delayed =
    route api context $ addParameterCheck delayed (withRequest go)
    where
      queryText :: Request -> QueryText
      queryText = queryToQueryText . queryString
      go :: Request -> DelayedIO [Filter tag a]
      go req = eitherToDelayed $ parseQueryText $ queryText req
      api = Proxy :: Proxy api
      eitherToDelayed = \case
        Left err -> delayedFailFatal err400 {errBody = T.encodeUtf8 $ T.fromStrict err}
        Right x -> pure x
