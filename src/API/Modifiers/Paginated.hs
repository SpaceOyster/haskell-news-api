{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module API.Modifiers.Paginated where

import App.Config as Config
import Data.Int
import Data.Maybe
import Effects.Config
import Servant

type Paginated = Offset :> Limit

type Offset = QueryParam "offset" Int32

type Limit = QueryParam "limit" Int32

getPagination :: MonadConfig m => Maybe Int32 -> Maybe Int32 -> m Pagination
getPagination mOffset mLimit = do
  pagination <- pagination . apiConfig <$> getConfig
  let offset = fromMaybe (Config.offset pagination) mOffset
      limit = fromMaybe (Config.limit pagination) mLimit
  pure $ Pagination {offset, limit}
