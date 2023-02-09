module GHC.TypeLits.Extended
  ( module GHC.TypeLits,
    symbolText,
    symbolCIText,
  )
where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Extended as T
import Data.Typeable (Proxy (..))
import GHC.TypeLits

symbolText :: (KnownSymbol a) => Proxy a -> T.Text
symbolText = T.pack . symbolVal

symbolCIText :: (KnownSymbol a) => Proxy a -> CI T.Text
symbolCIText = CI.mk . symbolText
