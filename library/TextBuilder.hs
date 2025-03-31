module TextBuilder
  ( TextBuilder,
    Builder,

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
    lazyText,
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

import qualified Data.Text.Lazy as TextLazy
import TextBuilder.Prelude hiding (intercalate, length, null)
import qualified TextBuilderDev as Dev

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
newtype TextBuilder
  = TextBuilder Dev.TextBuilder
  deriving (Show, IsString, Semigroup, Monoid)

{-# DEPRECATED Builder "Use TextBuilder instead" #-}

type Builder = TextBuilder

-- | Get the amount of characters
{-# INLINE length #-}
length :: TextBuilder -> Int
length = coerce Dev.length

-- | Check whether the builder is empty
{-# INLINE null #-}
null :: TextBuilder -> Bool
null = coerce Dev.null

-- | Execute a builder producing a strict text
run :: TextBuilder -> Text
run = coerce Dev.buildText

-- ** Output IO

-- | Put builder, to stdout
putToStdOut :: TextBuilder -> IO ()
putToStdOut = coerce Dev.putToStdOut

-- | Put builder, to stderr
putToStdErr :: TextBuilder -> IO ()
putToStdErr = coerce Dev.putToStdErr

-- | Put builder, followed by a line, to stdout
putLnToStdOut :: TextBuilder -> IO ()
putLnToStdOut = coerce Dev.putLnToStdOut

-- | Put builder, followed by a line, to stderr
putLnToStdErr :: TextBuilder -> IO ()
putLnToStdErr = coerce Dev.putLnToStdErr

-- * Constructors

-- | Unicode character
{-# INLINE char #-}
char :: Char -> TextBuilder
char = coerce Dev.char

-- | Unicode code point
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> TextBuilder
unicodeCodePoint = coerce Dev.unicodeCodePoint

-- | Single code-unit UTF-16 character
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> TextBuilder
utf16CodeUnits1 = coerce Dev.utf16CodeUnits1

-- | Double code-unit UTF-16 character
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> TextBuilder
utf16CodeUnits2 = coerce Dev.utf16CodeUnits2

-- | Single code-unit UTF-8 character
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> TextBuilder
utf8CodeUnits1 = coerce Dev.utf8CodeUnits1

-- | Double code-unit UTF-8 character
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> TextBuilder
utf8CodeUnits2 = coerce Dev.utf8CodeUnits2

-- | Triple code-unit UTF-8 character
{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits3 = coerce Dev.utf8CodeUnits3

-- | UTF-8 character out of 4 code units
{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits4 = coerce Dev.utf8CodeUnits4

-- | ASCII byte string
{-# INLINE asciiByteString #-}
asciiByteString :: ByteString -> TextBuilder
asciiByteString = coerce Dev.asciiByteString

-- | Strict text
{-# INLINE text #-}
text :: Text -> TextBuilder
text = coerce Dev.text

-- | Lazy text
{-# INLINE lazyText #-}
lazyText :: TextLazy.Text -> TextBuilder
lazyText = coerce Dev.lazyText

-- | String
{-# INLINE string #-}
string :: String -> TextBuilder
string = coerce Dev.string

-- | Decimal representation of an integral value
{-# INLINEABLE decimal #-}
decimal :: (Integral a) => a -> TextBuilder
decimal = coerce . Dev.decimal

-- | Decimal representation of an unsigned integral value
{-# INLINEABLE unsignedDecimal #-}
unsignedDecimal :: (Integral a) => a -> TextBuilder
unsignedDecimal = coerce . Dev.unsignedDecimal

-- | Decimal representation of an integral value with thousands separated by the specified character
{-# INLINEABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: (Integral a) => Char -> a -> TextBuilder
thousandSeparatedDecimal = fmap coerce . Dev.thousandSeparatedDecimal

-- | Decimal representation of an unsigned integral value with thousands separated by the specified character
{-# INLINEABLE thousandSeparatedUnsignedDecimal #-}
thousandSeparatedUnsignedDecimal :: (Integral a) => Char -> a -> TextBuilder
thousandSeparatedUnsignedDecimal = fmap coerce . Dev.thousandSeparatedUnsignedDecimal

-- | Data size in decimal notation over amount of bytes.
{-# INLINEABLE dataSizeInBytesInDecimal #-}
dataSizeInBytesInDecimal :: (Integral a) => Char -> a -> TextBuilder
dataSizeInBytesInDecimal = fmap coerce . Dev.dataSizeInBytesInDecimal

-- | Unsigned binary number
{-# INLINE unsignedBinary #-}
unsignedBinary :: (Integral a) => a -> TextBuilder
unsignedBinary = coerce . Dev.unsignedBinary

-- | Unsigned binary number
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> TextBuilder
unsignedPaddedBinary = coerce . Dev.unsignedPaddedBinary

-- | Hexadecimal representation of an integral value
{-# INLINE hexadecimal #-}
hexadecimal :: (Integral a) => a -> TextBuilder
hexadecimal = coerce . Dev.hexadecimal

-- | Unsigned hexadecimal representation of an integral value
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: (Integral a) => a -> TextBuilder
unsignedHexadecimal = coerce . Dev.unsignedHexadecimal

-- | Decimal digit
{-# INLINE decimalDigit #-}
decimalDigit :: (Integral a) => a -> TextBuilder
decimalDigit = coerce . Dev.decimalDigit

-- | Hexadecimal digit
{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: (Integral a) => a -> TextBuilder
hexadecimalDigit = coerce . Dev.hexadecimalDigit

-- | Intercalate builders
{-# INLINE intercalate #-}
intercalate :: (Foldable foldable) => TextBuilder -> foldable TextBuilder -> TextBuilder
intercalate a b = coerce (Dev.intercalate (coerce a) (foldr ((:) . coerce) [] b))

-- | Pad a builder from the left side to the specified length with the specified character
{-# INLINEABLE padFromLeft #-}
padFromLeft :: Int -> Char -> TextBuilder -> TextBuilder
padFromLeft = coerce Dev.padFromLeft

-- | Pad a builder from the right side to the specified length with the specified character
{-# INLINEABLE padFromRight #-}
padFromRight :: Int -> Char -> TextBuilder -> TextBuilder
padFromRight = coerce Dev.padFromRight

-- |
-- Time interval in seconds.
-- Directly applicable to 'DiffTime' and 'NominalDiffTime'.
{-# INLINEABLE intervalInSeconds #-}
intervalInSeconds :: (RealFrac seconds) => seconds -> TextBuilder
intervalInSeconds = coerce . Dev.intervalInSeconds

-- | Double with a fixed number of decimal places.
{-# INLINE fixedDouble #-}
fixedDouble ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
fixedDouble = coerce Dev.fixedDouble

-- | Double multiplied by 100 with a fixed number of decimal places applied and followed by a percent-sign.
{-# INLINE doublePercent #-}
doublePercent ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
doublePercent = coerce Dev.doublePercent

-- | Hexadecimal readable representation of binary data.
{-# INLINE hexData #-}
hexData :: ByteString -> TextBuilder
hexData = coerce Dev.hexData
