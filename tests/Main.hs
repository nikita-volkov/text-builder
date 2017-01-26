module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Text as A
import qualified Text.Builder.StrictText as B
import qualified Text.Builder.Char as C


main =
  defaultMain $
  testGroup "All tests" $
  [
    testProperty "Packing a list of chars is isomorphic to appending a list of char builders (strict)" $
    \chars ->
      A.pack chars ===
      B.charBuilder (foldMap C.char chars)
  ]
