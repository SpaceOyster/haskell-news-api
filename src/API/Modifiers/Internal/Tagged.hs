{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Modifiers.Internal.Tagged where

import GHC.Base (Symbol, Type)

data Tagged t = Tagged Symbol t

type (:?) = 'Tagged

type family UnTagged a where
  UnTagged ('Tagged _ a) = a

type family ListOfTags (a :: [Tagged Type]) :: [Symbol] where
  ListOfTags '[] = '[]
  ListOfTags ('Tagged tag a ': as) = tag ': ListOfTags as

type family GetTypeFor (a :: Symbol) (ts :: [Tagged Type]) where
  GetTypeFor tag ('Tagged tag a ': as) = a
  GetTypeFor tag ('Tagged tag' a ': as) = GetTypeFor tag as
