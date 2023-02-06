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
  ( (.:.),
    ColumnList (ColNil),
    Sorting (Ascend, Descend),
    SortableBy,
    SortingRequest (SortingRequest, unSortingRequest),
    ListOfTags,
    unSorting,
  )
where

import API.Modifiers.Internal
  ( ColumnList (ColNil),
    HasToBeInList,
    ListOfTags,
    ValidNamesList (),
    symbolCIText,
    (.:.),
  )
import qualified API.Modifiers.Internal as Internal
import Data.Bifunctor (first)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import Data.Typeable
import GHC.Base
import GHC.TypeLits
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Router
import qualified Text.Parsec as Parsec

data Sorting a = Ascend a | Descend a
  deriving (Eq, Show)

unSorting :: Sorting a -> a
unSorting (Ascend a) = a
unSorting (Descend a) = a

sortingParser :: Parsec.Parsec String st (a -> Sorting a)
sortingParser =
  asum
    [ Parsec.string "asc" >> pure Ascend,
      Parsec.string "desc" >> pure Descend
    ]

parseSorting :: T.Text -> Either T.Text (a -> Sorting a)
parseSorting =
  first T.tshow
    . Parsec.parse sortingParser "Pagination Order"
    . T.unpack
    . T.toLower

instance FromHttpApiData (a -> Sorting a) where
  parseUrlPiece = parseSorting

class ReifySorting (sorting :: Sorting Symbol) where
  reifySorting :: Sorting (CI T.Text)

instance (KnownSymbol a) => ReifySorting ('Ascend a) where
  reifySorting = Ascend $ symbolCIText $ Proxy @a

instance (KnownSymbol a) => ReifySorting ('Descend a) where
  reifySorting = Descend $ symbolCIText $ Proxy @a

type family ExtractColumnNameFromSorting (sorting :: Sorting Symbol) where
  ExtractColumnNameFromSorting ('Ascend a) = a
  ExtractColumnNameFromSorting ('Descend a) = a

newtype SortingRequest (available :: [Symbol]) = SortingRequest
  { unSortingRequest :: Sorting (CI T.Text)
  }

type DefaultColumnName (deflt :: Sorting Symbol) =
  ExtractColumnNameFromSorting deflt

type SortingSpec (available :: [Symbol]) (deflt :: Sorting Symbol) =
  ( HasToBeInList (DefaultColumnName deflt) available,
    ReifySorting deflt,
    ValidNamesList available
  ) ::
    Constraint

validateSortingName ::
  forall available deflt.
  SortingSpec (available :: [Symbol]) (deflt :: Sorting Symbol) =>
  (CI T.Text -> Sorting (CI T.Text)) ->
  CI T.Text ->
  Sorting (CI T.Text)
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
  type ServerT (SortableBy available deflt :> api) m = SortingRequest available -> ServerT api m

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
         in f . SortingRequest . validateSortingName @available @deflt ordering $ CI.mk sortField
