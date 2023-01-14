module API.Modifiers.Internal where

import GHC.TypeLits

data Tagged t = Tagged Symbol t
