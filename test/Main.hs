module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Text as A
import qualified Text.Builder as B
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text


main =
  defaultMain $
  testGroup "All tests" $
  [
    testProperty "ASCII ByteString" $ let
      gen = listOf $ do
        list <- listOf (choose (0, 127))
        return (ByteString.pack list)
      in forAll gen $ \ chunks ->
        mconcat chunks ===
        Text.encodeUtf8 (B.run (foldMap B.asciiByteString chunks))
    ,
    testProperty "Intercalation has the same effect as in Text" $
    \ separator texts ->
      A.intercalate separator texts ===
      B.run (B.intercalate (B.text separator) (fmap B.text texts))
    ,
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
    testCase "Separated thousands" $ do
      assertEqual "" "0" (B.run (B.thousandSeparatedUnsignedDecimal ',' 0))
      assertEqual "" "123" (B.run (B.thousandSeparatedUnsignedDecimal ',' 123))
      assertEqual "" "1,234" (B.run (B.thousandSeparatedUnsignedDecimal ',' 1234))
      assertEqual "" "1,234,567" (B.run (B.thousandSeparatedUnsignedDecimal ',' 1234567))
    ,
    testCase "Hexadecimal" $
    assertEqual "" "1f23" (B.run (B.hexadecimal 0x01f23))
    ,
    testCase "Negative Hexadecimal" $
    assertEqual "" "-1f23" (B.run (B.hexadecimal (-0x01f23)))
  ]
