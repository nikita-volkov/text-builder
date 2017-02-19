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
    testProperty "Packing a list of chars is isomorphic to appending a list of builders" $
    \chars ->
      A.pack chars ===
      B.run (foldMap B.char chars)
    ,
    testProperty "Concatting a list of texts is isomorphic to fold-mapping with builders" $
    \texts ->
      mconcat texts ===
      B.run (foldMap B.text texts)
    ,
    testProperty "Concatting a list of texts is isomorphic to concatting a list of builders" $
    \texts ->
      mconcat texts ===
      B.run (mconcat (map B.text texts))
  ]
