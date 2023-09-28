module Data.Text.Extended
  ( module Data.Text,
    tshow,
    textToLBS,
  )
where

import qualified Data.ByteString.Lazy as LBS (ByteString, fromStrict)
import Data.Text
import Data.Text.Encoding (encodeUtf8)

tshow :: (Show a) => a -> Text
tshow = pack . show

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . encodeUtf8
