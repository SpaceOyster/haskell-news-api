{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.News where

import App.Monad
import Data.Text
import Data.Aeson as A
import Data.Text.Extended as T
import Entities.Image
import Servant

type NewsAPI = Get '[JSON] Text :<|> PostCreated '[JSON] Text

news :: ServerT NewsAPI App
news = return "GET categories endpoint" :<|> return "POST categories endpoint"

newtype FileNameJSON = FileNameJSON {unFileNameJSON :: FileName}
  deriving (Show)

instance A.ToJSON FileNameJSON where
  toJSON (FileNameJSON fn) = A.String $ "/images/" <> T.tshow fn

instance A.FromJSON FileNameJSON where
  parseJSON = A.withText "FileNameJSON" (fmap FileNameJSON . parseFileName)
