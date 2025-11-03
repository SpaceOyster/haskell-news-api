module API.Modifiers.FilterableSpec (spec) where

import API.Modifiers.Filterable
import Data.Either
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Text.Parsec as Parsec

spec :: Spec
spec = describe "API.Modifiers.Filterable" $ do
  predicateParserSpec
  filterQueryKeyParserSpec
  doParseFilterQueryKeySpec

predicateParserSpec :: Spec
predicateParserSpec = describe "predicateParser" $ do
  prop "parses Predicate from legit string" $ do
    traverse (\x -> Parsec.runParser predicateParser () x (T.pack x)) predicateList `shouldSatisfy` isRight
  prop "fails to parse anything else, even upper case versions of legit strings" $
    \str -> do
      Parsec.runParser predicateParser () str (T.pack str) `shouldSatisfy` \x -> isLeft x || str `elem` predicateList

filterQueryKeyParserSpec :: Spec
filterQueryKeyParserSpec = describe "filterQueryKeyParser" $ do
  it "not sure how to test it" $
    pendingWith "not sure how to test it"

doParseFilterQueryKeySpec :: Spec
doParseFilterQueryKeySpec = describe "doParseFilterQueryKey" $ do
  it "not sure how to test it" $
    pendingWith "not sure how to test it"
