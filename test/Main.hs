module Main where

import qualified Data.ByteString as ByteString
import qualified Data.Text as A
import qualified Data.Text.Encoding as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified TextBuilder as B
import Prelude hiding (choose)

main :: IO ()
main =
  defaultMain
    $ testGroup "All tests"
    $ [ testProperty "ASCII ByteString"
          $ let gen = listOf $ do
                  list <- listOf (choose (0, 127))
                  return (ByteString.pack list)
             in forAll gen $ \chunks ->
                  mconcat chunks
                    === Text.encodeUtf8 (B.toText (foldMap B.asciiByteString chunks)),
        testProperty "Intercalation has the same effect as in Text"
          $ \separator texts ->
            A.intercalate separator texts
              === B.toText (B.intercalate (B.text separator) (fmap B.text texts)),
        testProperty "Packing a list of chars is isomorphic to appending a list of builders"
          $ \chars ->
            A.pack chars
              === B.toText (foldMap B.char chars),
        testProperty "Concatting a list of texts is isomorphic to fold-mapping with builders"
          $ \texts ->
            mconcat texts
              === B.toText (foldMap B.text texts),
        testProperty "Concatting a list of texts is isomorphic to concatting a list of builders"
          $ \texts ->
            mconcat texts
              === B.toText (mconcat (map B.text texts)),
        testProperty "Concatting a list of trimmed texts is isomorphic to concatting a list of builders"
          $ \texts ->
            let trimmedTexts = fmap (A.drop 3) texts
             in mconcat trimmedTexts
                  === B.toText (mconcat (map B.text trimmedTexts)),
        testProperty "Decimal" $ \(x :: Integer) ->
          (fromString . show) x === (B.toText (B.decimal x)),
        testProperty "Hexadecimal vs std show" $ \(x :: Integer) ->
          x >= 0 ==>
            (fromString . showHex x) "" === (B.toText . B.hexadecimal) x,
        testCase "Separated thousands" $ do
          assertEqual "" "0" (B.toText (B.thousandSeparatedUnsignedDecimal @Int ',' 0))
          assertEqual "" "123" (B.toText (B.thousandSeparatedUnsignedDecimal @Int ',' 123))
          assertEqual "" "1,234" (B.toText (B.thousandSeparatedUnsignedDecimal @Int ',' 1234))
          assertEqual "" "1,234,567" (B.toText (B.thousandSeparatedUnsignedDecimal @Int ',' 1234567)),
        testCase "Pad from left" $ do
          assertEqual "" "00" (B.toText (B.padFromLeft 2 '0' ""))
          assertEqual "" "00" (B.toText (B.padFromLeft 2 '0' "0"))
          assertEqual "" "01" (B.toText (B.padFromLeft 2 '0' "1"))
          assertEqual "" "12" (B.toText (B.padFromLeft 2 '0' "12"))
          assertEqual "" "123" (B.toText (B.padFromLeft 2 '0' "123")),
        testCase "Pad from right" $ do
          assertEqual "" "00" (B.toText (B.padFromRight 2 '0' ""))
          assertEqual "" "00" (B.toText (B.padFromRight 2 '0' "0"))
          assertEqual "" "10" (B.toText (B.padFromRight 2 '0' "1"))
          assertEqual "" "12" (B.toText (B.padFromRight 2 '0' "12"))
          assertEqual "" "123" (B.toText (B.padFromRight 2 '0' "123"))
          assertEqual "" "1  " (B.toText (B.padFromRight 3 ' ' "1")),
        testCase "Hexadecimal"
          $ assertEqual "" "1f23" (B.toText (B.hexadecimal @Int 0x01f23)),
        testCase "Negative Hexadecimal"
          $ assertEqual "" "-1f23" (B.toText (B.hexadecimal @Int (-0x01f23))),
        testGroup "Time interval"
          $ [ testCase "59s" $ assertEqual "" "00:00:00:59" $ B.toText $ B.intervalInSeconds @Rational 59,
              testCase "minute" $ assertEqual "" "00:00:01:00" $ B.toText $ B.intervalInSeconds @Rational 60,
              testCase "90s" $ assertEqual "" "00:00:01:30" $ B.toText $ B.intervalInSeconds @Rational 90,
              testCase "hour" $ assertEqual "" "00:01:00:00" $ B.toText $ B.intervalInSeconds @Rational 3600,
              testCase "day" $ assertEqual "" "01:00:00:00" $ B.toText $ B.intervalInSeconds @Rational 86400
            ],
        testCase "dataSizeInBytesInDecimal" $ do
          assertEqual "" "999B" (B.toText (B.dataSizeInBytesInDecimal @Int ',' 999))
          assertEqual "" "1kB" (B.toText (B.dataSizeInBytesInDecimal @Int ',' 1000))
          assertEqual "" "1.1kB" (B.toText (B.dataSizeInBytesInDecimal @Int ',' 1100))
          assertEqual "" "1.1MB" (B.toText (B.dataSizeInBytesInDecimal @Int ',' 1150000))
          assertEqual "" "9.9MB" (B.toText (B.dataSizeInBytesInDecimal @Int ',' 9990000))
          assertEqual "" "10MB" (B.toText (B.dataSizeInBytesInDecimal @Int ',' 10100000))
          assertEqual "" "1,000YB" (B.toText (B.dataSizeInBytesInDecimal @Integer ',' 1000000000000000000000000000))
      ]
