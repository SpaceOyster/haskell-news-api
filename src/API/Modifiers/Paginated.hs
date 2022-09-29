{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Modifiers.Paginated where

import App.Config as Config
import Data.Maybe
import Effects.Config
import Servant

type Offset = QueryParam "offset" Integer

type Limit = QueryParam "limit" Integer

type family Paginated sub where
  Paginated sub = Offset :> Limit :> sub

getPagination :: MonadConfig m => Maybe Integer -> Maybe Integer -> m Pagination
getPagination mOffset mLimit = do
  pagination <- pagination . apiConfig <$> getConfig
  let offset = fromMaybe (Config.offset pagination) mOffset
      limit = fromMaybe (Config.limit pagination) mLimit
  pure $ Pagination {offset, limit}
