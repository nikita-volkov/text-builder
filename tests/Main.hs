module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Text as A
import qualified Text.Builder as B


main =
  defaultMain $
  testGroup "All tests" $
  [
    testProperty "Packing a list of chars is isomorphic to appending a list of char builders (strict)" $
    \chars ->
      A.pack chars ===
      B.run (foldMap B.char chars)
  ]
