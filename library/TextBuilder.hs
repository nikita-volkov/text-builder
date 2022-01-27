module TextBuilder
  ( TextBuilder,

    -- * Accessors
    buildText,
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
    utcTimestampInIso8601,
    intervalInSeconds,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Text.Array as B
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.IO as Text
import qualified Data.Text.Internal as C
import qualified DeferredFolds.Unfoldr as Unfoldr
import TextBuilder.Prelude hiding (intercalate, length, null)
import qualified TextBuilder.UTF16 as D

newtype Action
  = Action (forall s. B.MArray s -> Int -> ST s ())

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data TextBuilder
  = TextBuilder !Action !Int !Int

instance Monoid TextBuilder where
  {-# INLINE mempty #-}
  mempty =
    TextBuilder (Action (\_ _ -> return ())) 0 0
  {-# INLINEABLE mappend #-}
  mappend (TextBuilder (Action action1) arraySize1 charsAmount1) (TextBuilder (Action action2) arraySize2 charsAmount2) =
    TextBuilder action arraySize charsAmount
    where
      action =
        Action $ \array offset -> do
          action1 array offset
          action2 array (offset + arraySize1)
      arraySize =
        arraySize1 + arraySize2
      charsAmount =
        charsAmount1 + charsAmount2

instance Semigroup TextBuilder where
  (<>) = mappend

instance IsString TextBuilder where
  fromString = string

instance Show TextBuilder where
  show = Text.unpack . buildText

instance FromText TextBuilder where
  fromText = TextBuilder.text

instance ToText TextBuilder where
  toText = buildText

instance ToString TextBuilder where
  toString = toString . buildText

-- * Accessors

-- | Get the amount of characters
{-# INLINE length #-}
length :: TextBuilder -> Int
length (TextBuilder _ _ x) = x

-- | Check whether the builder is empty
{-# INLINE null #-}
null :: TextBuilder -> Bool
null = (== 0) . length

-- | Execute a builder producing a strict text
buildText :: TextBuilder -> Text
buildText (TextBuilder (Action action) arraySize _) =
  C.text array 0 arraySize
  where
    array =
      runST $ do
        array <- B.new arraySize
        action array 0
        B.unsafeFreeze array

-- ** Output IO

-- | Put builder, to stdout
putToStdOut :: TextBuilder -> IO ()
putToStdOut = Text.hPutStr stdout . buildText

-- | Put builder, to stderr
putToStdErr :: TextBuilder -> IO ()
putToStdErr = Text.hPutStr stderr . buildText

-- | Put builder, followed by a line, to stdout
putLnToStdOut :: TextBuilder -> IO ()
putLnToStdOut = Text.hPutStrLn stdout . buildText

-- | Put builder, followed by a line, to stderr
putLnToStdErr :: TextBuilder -> IO ()
putLnToStdErr = Text.hPutStrLn stderr . buildText

-- * Constructors

-- | Unicode character
{-# INLINE char #-}
char :: Char -> TextBuilder
char x =
  unicodeCodePoint (ord x)

-- | Unicode code point
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> TextBuilder
unicodeCodePoint x =
  D.unicodeCodePoint x utf16CodeUnits1 utf16CodeUnits2

-- | Single code-unit UTF-16 character
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> TextBuilder
utf16CodeUnits1 unit =
  TextBuilder action 1 1
  where
    action =
      Action $ \array offset -> B.unsafeWrite array offset unit

-- | Double code-unit UTF-16 character
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> TextBuilder
utf16CodeUnits2 unit1 unit2 =
  TextBuilder action 2 1
  where
    action =
      Action $ \array offset -> do
        B.unsafeWrite array offset unit1
        B.unsafeWrite array (succ offset) unit2

-- | Single code-unit UTF-8 character
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> TextBuilder
utf8CodeUnits1 unit1 =
  D.utf8CodeUnits1 unit1 utf16CodeUnits1 utf16CodeUnits2

-- | Double code-unit UTF-8 character
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> TextBuilder
utf8CodeUnits2 unit1 unit2 =
  D.utf8CodeUnits2 unit1 unit2 utf16CodeUnits1 utf16CodeUnits2

-- | Triple code-unit UTF-8 character
{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits3 unit1 unit2 unit3 =
  D.utf8CodeUnits3 unit1 unit2 unit3 utf16CodeUnits1 utf16CodeUnits2

-- | UTF-8 character out of 4 code units
{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits4 unit1 unit2 unit3 unit4 =
  D.utf8CodeUnits4 unit1 unit2 unit3 unit4 utf16CodeUnits1 utf16CodeUnits2

-- | ASCII byte string
{-# INLINEABLE asciiByteString #-}
asciiByteString :: ByteString -> TextBuilder
asciiByteString byteString =
  TextBuilder action length length
  where
    length = ByteString.length byteString
    action =
      Action $ \array ->
        let step byte next index = do
              B.unsafeWrite array index (fromIntegral byte)
              next (succ index)
         in ByteString.foldr step (const (return ())) byteString

-- | Strict text
{-# INLINEABLE text #-}
text :: Text -> TextBuilder
text text@(C.Text array offset length) =
  TextBuilder action length (Text.length text)
  where
    action =
      Action $ \builderArray builderOffset -> do
        B.copyI builderArray builderOffset array offset (builderOffset + length)

-- | String
{-# INLINE string #-}
string :: String -> TextBuilder
string =
  foldMap char

-- | Decimal representation of an integral value
{-# INLINEABLE decimal #-}
decimal :: Integral a => a -> TextBuilder
decimal i =
  if i >= 0
    then unsignedDecimal i
    else unicodeCodePoint 45 <> unsignedDecimal (negate i)

-- | Decimal representation of an unsigned integral value
{-# INLINEABLE unsignedDecimal #-}
unsignedDecimal :: Integral a => a -> TextBuilder
unsignedDecimal =
  foldMap decimalDigit . Unfoldr.decimalDigits

-- | Decimal representation of an integral value with thousands separated by the specified character
{-# INLINEABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: Integral a => Char -> a -> TextBuilder
thousandSeparatedDecimal separatorChar a =
  if a >= 0
    then thousandSeparatedUnsignedDecimal separatorChar a
    else unicodeCodePoint 45 <> thousandSeparatedUnsignedDecimal separatorChar (negate a)

-- | Decimal representation of an unsigned integral value with thousands separated by the specified character
{-# INLINEABLE thousandSeparatedUnsignedDecimal #-}
thousandSeparatedUnsignedDecimal :: Integral a => Char -> a -> TextBuilder
thousandSeparatedUnsignedDecimal separatorChar a =
  fold $ do
    (index, digit) <- Unfoldr.zipWithReverseIndex $ Unfoldr.decimalDigits a
    if mod index 3 == 0 && index /= 0
      then return (decimalDigit digit <> char separatorChar)
      else return (decimalDigit digit)

-- | Data size in decimal notation over amount of bytes.
{-# INLINEABLE dataSizeInBytesInDecimal #-}
dataSizeInBytesInDecimal :: Integral a => Char -> a -> TextBuilder
dataSizeInBytesInDecimal separatorChar amount =
  if amount < 1000
    then unsignedDecimal amount <> "B"
    else
      if amount < 1000000
        then dividedDecimal separatorChar 100 amount <> "kB"
        else
          if amount < 1000000000
            then dividedDecimal separatorChar 100000 amount <> "MB"
            else
              if amount < 1000000000000
                then dividedDecimal separatorChar 100000000 amount <> "GB"
                else
                  if amount < 1000000000000000
                    then dividedDecimal separatorChar 100000000000 amount <> "TB"
                    else
                      if amount < 1000000000000000000
                        then dividedDecimal separatorChar 100000000000000 amount <> "PB"
                        else
                          if amount < 1000000000000000000000
                            then dividedDecimal separatorChar 100000000000000000 amount <> "EB"
                            else
                              if amount < 1000000000000000000000000
                                then dividedDecimal separatorChar 100000000000000000000 amount <> "ZB"
                                else dividedDecimal separatorChar 100000000000000000000000 amount <> "YB"

dividedDecimal :: Integral a => Char -> a -> a -> TextBuilder
dividedDecimal separatorChar divisor n =
  let byDivisor = div n divisor
      byExtraTen = div byDivisor 10
      remainder = byDivisor - byExtraTen * 10
   in if remainder == 0 || byExtraTen >= 10
        then thousandSeparatedDecimal separatorChar byExtraTen
        else thousandSeparatedDecimal separatorChar byExtraTen <> "." <> decimalDigit remainder

-- | Unsigned binary number
{-# INLINE unsignedBinary #-}
unsignedBinary :: Integral a => a -> TextBuilder
unsignedBinary =
  foldMap decimalDigit . Unfoldr.binaryDigits

-- | Unsigned binary number
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> TextBuilder
unsignedPaddedBinary a =
  padFromLeft (finiteBitSize a) '0' $ foldMap decimalDigit $ Unfoldr.binaryDigits a

-- | Hexadecimal representation of an integral value
{-# INLINE hexadecimal #-}
hexadecimal :: Integral a => a -> TextBuilder
hexadecimal i =
  if i >= 0
    then unsignedHexadecimal i
    else unicodeCodePoint 45 <> unsignedHexadecimal (negate i)

-- | Unsigned hexadecimal representation of an integral value
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: Integral a => a -> TextBuilder
unsignedHexadecimal =
  foldMap hexadecimalDigit . Unfoldr.hexadecimalDigits

-- | Decimal digit
{-# INLINE decimalDigit #-}
decimalDigit :: Integral a => a -> TextBuilder
decimalDigit n =
  unicodeCodePoint (fromIntegral n + 48)

-- | Hexadecimal digit
{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Integral a => a -> TextBuilder
hexadecimalDigit n =
  if n <= 9
    then unicodeCodePoint (fromIntegral n + 48)
    else unicodeCodePoint (fromIntegral n + 87)

-- | Intercalate builders
{-# INLINE intercalate #-}
intercalate :: Foldable foldable => TextBuilder -> foldable TextBuilder -> TextBuilder
intercalate separator = extract . foldl' step init
  where
    init = Product2 False mempty
    step (Product2 isNotFirst builder) element =
      Product2 True $
        if isNotFirst
          then builder <> separator <> element
          else element
    extract (Product2 _ builder) = builder

-- | Pad a builder from the left side to the specified length with the specified character
{-# INLINEABLE padFromLeft #-}
padFromLeft :: Int -> Char -> TextBuilder -> TextBuilder
padFromLeft paddedLength paddingChar builder =
  let builderLength = length builder
   in if paddedLength <= builderLength
        then builder
        else foldMap char (replicate (paddedLength - builderLength) paddingChar) <> builder

-- | Pad a builder from the right side to the specified length with the specified character
{-# INLINEABLE padFromRight #-}
padFromRight :: Int -> Char -> TextBuilder -> TextBuilder
padFromRight paddedLength paddingChar builder =
  let builderLength = length builder
   in if paddedLength <= builderLength
        then builder
        else builder <> foldMap char (replicate (paddedLength - builderLength) paddingChar)

-- |
-- General template for formatting date values according to the ISO8601 standard.
-- The format is the following:
--
-- > 2021-11-24T12:11:02Z
--
-- Integrations with various time-libraries can be easily derived from that.
utcTimestampInIso8601 ::
  -- | Year.
  Int ->
  -- | Month.
  Int ->
  -- | Day.
  Int ->
  -- | Hour.
  Int ->
  -- | Minute.
  Int ->
  -- | Second.
  Int ->
  TextBuilder
utcTimestampInIso8601 y mo d h mi s =
  mconcat
    [ padFromLeft 4 '0' $ decimal y,
      "-",
      padFromLeft 2 '0' $ decimal mo,
      "-",
      padFromLeft 2 '0' $ decimal d,
      "T",
      padFromLeft 2 '0' $ decimal h,
      ":",
      padFromLeft 2 '0' $ decimal mi,
      ":",
      padFromLeft 2 '0' $ decimal s,
      "Z"
    ]

-- |
-- Time interval in seconds.
-- Directly applicable to 'DiffTime' and 'NominalDiffTime'.
{-# INLINEABLE intervalInSeconds #-}
intervalInSeconds :: RealFrac seconds => seconds -> TextBuilder
intervalInSeconds interval = flip evalState (round interval) $ do
  seconds <- state (swap . flip divMod 60)
  minutes <- state (swap . flip divMod 60)
  hours <- state (swap . flip divMod 24)
  days <- get
  return $
    padFromLeft 2 '0' (decimal days) <> ":"
      <> padFromLeft 2 '0' (decimal hours)
      <> ":"
      <> padFromLeft 2 '0' (decimal minutes)
      <> ":"
      <> padFromLeft 2 '0' (decimal seconds)

-- | Double with a fixed number of decimal places.
{-# INLINE fixedDouble #-}
fixedDouble ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
fixedDouble decimalPlaces = fromString . printf ("!." ++ show decimalPlaces ++ "f")

-- | Double multiplied by 100 with a fixed number of decimal places applied and followed by a percent-sign.
{-# INLINE doublePercent #-}
doublePercent ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
doublePercent decimalPlaces x = fixedDouble decimalPlaces (x * 100) <> "!"

-- | Hexadecimal readable representation of binary data.
{-# INLINE hexData #-}
hexData :: ByteString -> TextBuilder
hexData =
  intercalate " " . fmap mconcat
    . Split.chunksOf 2
    . fmap byte
    . ByteString.unpack
  where
    byte =
      padFromLeft 2 '0' . unsignedHexadecimal
