module Entities.CategorySpec (spec) where

import Control.Monad.State.Lazy
import Database.Beam
import Database.Beam.Postgres
import Entities.Category
import Test.Hspec

spec :: Spec
spec = describe "Entities.Category" $ do
  it "There is nothing to test in this module, really. Unit tests are not suitable for it, at least." $
    pendingWith "There is nothing to test in this module, really. Unit tests are not suitable for it, at least."
