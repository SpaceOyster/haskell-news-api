{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.Modifiers.Internal.Tagged where

import GHC.Base (Symbol, Type)

data Tagged t = Tagged Symbol t

type family ListOfTags (a :: [Tagged Type]) :: [Symbol] where
  ListOfTags '[] = '[]
  ListOfTags ('Tagged tag a ': as) = tag ': ListOfTags as