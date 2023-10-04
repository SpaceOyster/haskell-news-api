{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Sortable
  ( Sorting (Ascend, Descend),
    SortableBy,
    SortingRequest (SortingRequest, unSortingRequest),
    ListOfTags,
    unSorting,
    UnSorting,
    SortingHasToBeAvailable,
    SortingIsAvailable,
    ReifySorting (..),
  )
where

import API.Modifiers.Internal
  ( ListOfTags,
  )
import API.Modifiers.Internal.PolyKinds as Internal
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import Data.Typeable
import GHC.Base
import GHC.TypeLits.Extended
import Servant
import qualified Servant.Docs.Internal as Docs
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router
import qualified Text.Parsec as Parsec

data Sorting a = Ascend a | Descend a
  deriving (Eq, Show)

unSorting :: Sorting a -> a
unSorting (Ascend a) = a
unSorting (Descend a) = a

type family UnSorting a where
  UnSorting ('Ascend a) = a
  UnSorting ('Descend a) = a

showSortingOrder :: Sorting a -> String
showSortingOrder (Ascend _) = "asc"
showSortingOrder (Descend _) = "desc"

sortingParser :: Parsec.Parsec T.Text st (a -> Sorting a)
sortingParser =
  Parsec.choice
    [ Parsec.string "asc" >> pure Ascend,
      Parsec.string "desc" >> pure Descend
    ]

parseSorting :: T.Text -> Either T.Text (a -> Sorting a)
parseSorting =
  first T.tshow
    . Parsec.parse sortingParser "Pagination Order"
    . T.toLower

instance FromHttpApiData (a -> Sorting a) where
  parseUrlPiece = parseSorting

class ReifySorting (sorting :: Sorting Symbol) where
  reifySorting :: Sorting T.Text

instance (KnownSymbol a) => ReifySorting ('Ascend a) where
  reifySorting = Ascend $ symbolText $ Proxy @a

instance (KnownSymbol a) => ReifySorting ('Descend a) where
  reifySorting = Descend $ symbolText $ Proxy @a

type ExtractColumnNameFromSorting (sorting :: Sorting Symbol) = UnSorting sorting

newtype SortingRequest (available :: [Symbol]) (deflt :: Sorting Symbol) = SortingRequest
  { unSortingRequest :: Sorting T.Text
  }

type SortingIsAvailable (sort :: Sorting Symbol) (avail :: [Symbol]) =
  (Internal.Elem (ExtractColumnNameFromSorting sort) avail) :: Bool

type SortingHasToBeAvailable (sort :: Sorting Symbol) (avail :: [Symbol]) =
  (HasToBeInList (ExtractColumnNameFromSorting sort) avail) :: Constraint


type SortingSpec (available :: [Symbol]) (deflt :: Sorting Symbol) =
  ( SortingHasToBeAvailable deflt available,
    ReifySorting deflt,
    ValidNamesList available
  ) ::
    Constraint

validateSortingName ::
  forall available deflt.
  (SortingSpec (available :: [Symbol]) (deflt :: Sorting Symbol)) =>
  (T.Text -> Sorting T.Text) ->
  T.Text ->
  Sorting T.Text
validateSortingName ordering name =
  if Internal.isNameValid @available name
    then ordering name
    else reifySorting @deflt

data SortableBy (available :: [Symbol]) (deflt :: Sorting Symbol)

instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    SortingSpec available deflt,
    ReifySorting deflt
  ) =>
  HasServer (SortableBy available deflt :> api) context
  where
  type ServerT (SortableBy available deflt :> api) m = SortingRequest available deflt -> ServerT api m

  hoistServerWithContext ::
    Proxy (SortableBy available deflt :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    ServerT (SortableBy available deflt :> api) m ->
    ServerT (SortableBy available deflt :> api) n
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route ::
    ( SortingSpec available deflt,
      ReifySorting deflt
    ) =>
    Proxy (SortableBy available deflt :> api) ->
    Context context ->
    Delayed env (Server (SortableBy available deflt :> api)) ->
    Router env
  route Proxy context delayed =
    route api context (provideSorting <$> delayed)
    where
      api = Proxy :: Proxy (QueryParam "order" (a -> Sorting a) :> QueryParam "sort-by" T.Text :> api)
      provideSorting f mOrder mSortBy =
        let ordering = fromMaybe Ascend mOrder
            sortField = fromMaybe (T.pack "") mSortBy
         in f . SortingRequest . validateSortingName @available @deflt ordering $ sortField

instance
  ( Docs.HasDocs api,
    ReifySorting deflt,
    Internal.ReifySymbolsList available
  ) =>
  Docs.HasDocs (SortableBy available deflt :> api)
  where
  docsFor Proxy (endpoint, action) = Docs.docsFor subAPI (endpoint, action')
    where
      subAPI = Proxy :: Proxy api
      action' = action {Docs._params = Docs._params action <> docQParam}
      availableFieldNames = reifySymbolsList (Proxy @available)
      defltSorting = reifySorting @deflt
      defltFieldName = T.unpack (unSorting defltSorting) :: String
      defltOrder = showSortingOrder defltSorting :: String
      docQParam =
        [ Docs.DocQueryParam
            { Docs._paramName = "sort-by",
              Docs._paramValues = availableFieldNames,
              Docs._paramDesc = "A name of field to sort returned items by. Default is " <> defltFieldName,
              Docs._paramKind = Docs.Normal
            },
          Docs.DocQueryParam
            { Docs._paramName = "order",
              Docs._paramValues = ["asc", "desc"],
              Docs._paramDesc = "An sorting for returned items. Default is " <> defltOrder,
              Docs._paramKind = Docs.Normal
            }
        ]
