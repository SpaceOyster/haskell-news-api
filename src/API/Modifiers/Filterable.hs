{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Filterable
  ( FilterableBy,
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
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive as CI (mk)
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

data FilteringRequest a where
  FiltReqNil :: FilteringRequest '[]
  FiltReqCons :: [Filter name typ] -> FilteringRequest as -> FilteringRequest ('Tagged name typ ': as)

infixr 3 `FiltReqCons`

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

instance {-# OVERLAPPABLE #-} (FromHttpApiData a) => FilterValue a where
  parseFilterValue = parseQueryParam

instance {-# OVERLAPS #-} FilterValue (CI String) where
  parseFilterValue = fmap CI.mk . parseQueryParam

instance {-# OVERLAPS #-} FilterValue (CI T.Text) where
  parseFilterValue = fmap CI.mk . parseQueryParam

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

class FiltersList filters where
  parseFiltersFromQueryText :: QueryText -> Either T.Text (FilteringRequest filters)

instance FiltersList '[] where
  parseFiltersFromQueryText _ = Right FiltReqNil

instance
  (KnownSymbol ftag, FilterValue ftyp, FiltersList fs) =>
  FiltersList ('Tagged ftag ftyp ': fs)
  where
  parseFiltersFromQueryText qt = do
    eFilters <- parseQueryText @ftag @ftyp qt
    eRemainFilt <- parseFiltersFromQueryText @fs qt
    return $ eFilters `FiltReqCons` eRemainFilt

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

data FilterableBy (a :: [Tagged Type])

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    FiltersList filters
  ) =>
  HasServer (FilterableBy filters :> api) context
  where
  type ServerT (FilterableBy filters :> api) m = FilteringRequest filters -> ServerT api m

  hoistServerWithContext ::
    Proxy (FilterableBy filters :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (FilterableBy filters :> api) m ->
    ServerT (FilterableBy filters :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    Proxy (FilterableBy filters :> api) ->
    Context context ->
    Delayed env (Server (FilterableBy filters :> api)) ->
    Router env
  route Proxy context delayed =
    route api context $ addParameterCheck delayed (withRequest go)
    where
      queryText :: Request -> QueryText
      queryText = queryToQueryText . queryString
      go :: Request -> DelayedIO (FilteringRequest filters)
      go req = eitherToDelayed $ parseFiltersFromQueryText $ queryText req
      api = Proxy :: Proxy api
      eitherToDelayed = \case
        Left err -> delayedFailFatal err400 {errBody = T.encodeUtf8 $ T.fromStrict err}
        Right x -> pure x
