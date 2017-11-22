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
    \ chars ->
      A.pack chars ===
      B.run (foldMap B.char chars)
    ,
    testProperty "Concatting a list of texts is isomorphic to fold-mapping with builders" $
    \ texts ->
      mconcat texts ===
      B.run (foldMap B.text texts)
    ,
    testProperty "Concatting a list of texts is isomorphic to concatting a list of builders" $
    \ texts ->
      mconcat texts ===
      B.run (mconcat (map B.text texts))
    ,
    testProperty "Concatting a list of trimmed texts is isomorphic to concatting a list of builders" $
    \ texts ->
      let
        trimmedTexts = fmap (A.drop 3) texts
        in
          mconcat trimmedTexts ===
          B.run (mconcat (map B.text trimmedTexts))
    ,
    testProperty "Decimal" $ \ (x :: Integer) ->
    (fromString . show) x === (B.run (B.decimal x))
    ,
    testProperty "Hexadecimal vs std show" $ \ (x :: Integer) -> x >= 0 ==>
    (fromString . showHex x) "" === (B.run . B.hexadecimal) x
    ,
    testCase "Hexadecimal" $
    assertEqual "" "1f23" (B.run (B.hexadecimal 0x01f23))
    ,
    testCase "Negative Hexadecimal" $
    assertEqual "" "-1f23" (B.run (B.hexadecimal (-0x01f23)))
  ]
