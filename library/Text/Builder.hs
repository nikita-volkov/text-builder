module Text.Builder
  ( Builder,

    -- * Accessors
    run,
    length,
    null,

    -- ** Output IO
    putToStdOut,
    putToStdErr,
    putLnToStdOut,
    putLnToStdErr,

    -- * Constructors

    -- ** Builder manipulators
    intercalate,
    padFromLeft,
    padFromRight,

    -- ** Textual
    text,
    string,
    asciiByteString,
    hexData,

    -- ** Character
    char,

    -- *** Low-level character
    unicodeCodePoint,
    utf16CodeUnits1,
    utf16CodeUnits2,
    utf8CodeUnits1,
    utf8CodeUnits2,
    utf8CodeUnits3,
    utf8CodeUnits4,

    -- ** Integers

    -- *** Decimal
    decimal,
    unsignedDecimal,
    thousandSeparatedDecimal,
    thousandSeparatedUnsignedDecimal,
    dataSizeInBytesInDecimal,

    -- *** Binary
    unsignedBinary,
    unsignedPaddedBinary,

    -- *** Hexadecimal
    hexadecimal,
    unsignedHexadecimal,

    -- ** Digits
    decimalDigit,
    hexadecimalDigit,

    -- ** Real
    fixedDouble,
    doublePercent,

    -- ** Time
    intervalInSeconds,
  )
where

import BasePrelude hiding (intercalate, length, null)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified TextBuilderDev as Dev

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
type Builder = Dev.TextBuilder

-- | Get the amount of characters
{-# INLINE length #-}
length :: Builder -> Int
length = Dev.length

-- | Check whether the builder is empty
{-# INLINE null #-}
null :: Builder -> Bool
null = Dev.null

-- | Execute a builder producing a strict text
run :: Builder -> Text
run = Dev.buildText

-- ** Output IO

-- | Put builder, to stdout
putToStdOut :: Builder -> IO ()
putToStdOut = Dev.putToStdOut

-- | Put builder, to stderr
putToStdErr :: Builder -> IO ()
putToStdErr = Dev.putToStdErr

-- | Put builder, followed by a line, to stdout
putLnToStdOut :: Builder -> IO ()
putLnToStdOut = Dev.putLnToStdOut

-- | Put builder, followed by a line, to stderr
putLnToStdErr :: Builder -> IO ()
putLnToStdErr = Dev.putLnToStdErr

-- * Constructors

-- | Unicode character
{-# INLINE char #-}
char :: Char -> Builder
char = Dev.char

-- | Unicode code point
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> Builder
unicodeCodePoint = Dev.unicodeCodePoint

-- | Single code-unit UTF-16 character
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> Builder
utf16CodeUnits1 = Dev.utf16CodeUnits1

-- | Double code-unit UTF-16 character
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> Builder
utf16CodeUnits2 = Dev.utf16CodeUnits2

-- | Single code-unit UTF-8 character
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> Builder
utf8CodeUnits1 = Dev.utf8CodeUnits1

-- | Double code-unit UTF-8 character
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> Builder
utf8CodeUnits2 = Dev.utf8CodeUnits2

-- | Triple code-unit UTF-8 character
{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits3 = Dev.utf8CodeUnits3

-- | UTF-8 character out of 4 code units
{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits4 = Dev.utf8CodeUnits4

-- | ASCII byte string
{-# INLINEABLE asciiByteString #-}
asciiByteString :: ByteString -> Builder
asciiByteString = Dev.asciiByteString

-- | Strict text
{-# INLINEABLE text #-}
text :: Text -> Builder
text = Dev.text

-- | String
{-# INLINE string #-}
string :: String -> Builder
string = Dev.string

-- | Decimal representation of an integral value
{-# INLINEABLE decimal #-}
decimal :: Integral a => a -> Builder
decimal = Dev.decimal

-- | Decimal representation of an unsigned integral value
{-# INLINEABLE unsignedDecimal #-}
unsignedDecimal :: Integral a => a -> Builder
unsignedDecimal = Dev.unsignedDecimal

-- | Decimal representation of an integral value with thousands separated by the specified character
{-# INLINEABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: Integral a => Char -> a -> Builder
thousandSeparatedDecimal = Dev.thousandSeparatedDecimal

-- | Decimal representation of an unsigned integral value with thousands separated by the specified character
{-# INLINEABLE thousandSeparatedUnsignedDecimal #-}
thousandSeparatedUnsignedDecimal :: Integral a => Char -> a -> Builder
thousandSeparatedUnsignedDecimal = Dev.thousandSeparatedUnsignedDecimal

-- | Data size in decimal notation over amount of bytes.
{-# INLINEABLE dataSizeInBytesInDecimal #-}
dataSizeInBytesInDecimal :: Integral a => Char -> a -> Builder
dataSizeInBytesInDecimal = Dev.dataSizeInBytesInDecimal

-- | Unsigned binary number
{-# INLINE unsignedBinary #-}
unsignedBinary :: Integral a => a -> Builder
unsignedBinary = Dev.unsignedBinary

-- | Unsigned binary number
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> Builder
unsignedPaddedBinary = Dev.unsignedPaddedBinary

-- | Hexadecimal representation of an integral value
{-# INLINE hexadecimal #-}
hexadecimal :: Integral a => a -> Builder
hexadecimal = Dev.hexadecimal

-- | Unsigned hexadecimal representation of an integral value
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: Integral a => a -> Builder
unsignedHexadecimal = Dev.unsignedHexadecimal

-- | Decimal digit
{-# INLINE decimalDigit #-}
decimalDigit :: Integral a => a -> Builder
decimalDigit = Dev.decimalDigit

-- | Hexadecimal digit
{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Integral a => a -> Builder
hexadecimalDigit = Dev.hexadecimalDigit

-- | Intercalate builders
{-# INLINE intercalate #-}
intercalate :: Foldable foldable => Builder -> foldable Builder -> Builder
intercalate = Dev.intercalate

-- | Pad a builder from the left side to the specified length with the specified character
{-# INLINEABLE padFromLeft #-}
padFromLeft :: Int -> Char -> Builder -> Builder
padFromLeft = Dev.padFromLeft

-- | Pad a builder from the right side to the specified length with the specified character
{-# INLINEABLE padFromRight #-}
padFromRight :: Int -> Char -> Builder -> Builder
padFromRight = Dev.padFromRight

-- |
-- Time interval in seconds.
-- Directly applicable to 'DiffTime' and 'NominalDiffTime'.
{-# INLINEABLE intervalInSeconds #-}
intervalInSeconds :: RealFrac seconds => seconds -> Builder
intervalInSeconds = Dev.intervalInSeconds

-- | Double with a fixed number of decimal places.
{-# INLINE fixedDouble #-}
fixedDouble ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  Builder
fixedDouble = Dev.fixedDouble

-- | Double multiplied by 100 with a fixed number of decimal places applied and followed by a percent-sign.
{-# INLINE doublePercent #-}
doublePercent ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  Builder
doublePercent = Dev.doublePercent

-- | Hexadecimal readable representation of binary data.
{-# INLINE hexData #-}
hexData :: ByteString -> Builder
hexData = Dev.hexData
