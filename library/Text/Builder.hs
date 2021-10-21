module Text.Builder
(
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

import Text.Builder.Prelude hiding (length, null, intercalate)
import qualified Data.Text.Array as B
import qualified Data.Text.Internal as C
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Text.Builder.UTF16 as D
import qualified Data.ByteString as ByteString
import qualified DeferredFolds.Unfoldr as Unfoldr
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List.Split as Split


newtype Action =
  Action (forall s. B.MArray s -> Int -> ST s ())

{-|
Specification of how to efficiently construct strict 'Text'.
Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
-}
data Builder =
  Builder !Action !Int !Int

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Builder (Action (\_ _ -> return ())) 0 0
  {-# INLINABLE mappend #-}
  mappend (Builder (Action action1) arraySize1 charsAmount1) (Builder (Action action2) arraySize2 charsAmount2) =
    Builder action arraySize charsAmount
    where
      action =
        Action $ \array offset -> do
          action1 array offset
          action2 array (offset + arraySize1)
      arraySize =
        arraySize1 + arraySize2
      charsAmount =
        charsAmount1 + charsAmount2

instance Semigroup Builder where
  (<>) = mappend

instance IsString Builder where
  fromString = string

instance Show Builder where
  show = Text.unpack . run


-- * Accessors
-------------------------

{-| Get the amount of characters -}
{-# INLINE length #-}
length :: Builder -> Int
length (Builder _ _ x) = x

{-| Check whether the builder is empty -}
{-# INLINE null #-}
null :: Builder -> Bool
null = (== 0) . length

{-| Execute a builder producing a strict text -}
run :: Builder -> Text
run (Builder (Action action) arraySize _) =
  C.text array 0 arraySize
  where
    array =
      runST $ do
        array <- B.new arraySize
        action array 0
        B.unsafeFreeze array

-- ** Output IO
-------------------------

{-| Put builder, to stdout -}
putToStdOut :: Builder -> IO ()
putToStdOut = Text.hPutStr stdout . run

{-| Put builder, to stderr -}
putToStdErr :: Builder -> IO ()
putToStdErr = Text.hPutStr stderr . run

{-| Put builder, followed by a line, to stdout -}
putLnToStdOut :: Builder -> IO ()
putLnToStdOut = Text.hPutStrLn stdout . run

{-| Put builder, followed by a line, to stderr -}
putLnToStdErr :: Builder -> IO ()
putLnToStdErr = Text.hPutStrLn stderr . run


-- * Constructors
-------------------------

{-| Unicode character -}
{-# INLINE char #-}
char :: Char -> Builder
char x =
  unicodeCodePoint (ord x)

{-| Unicode code point-}
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> Builder
unicodeCodePoint x =
  D.unicodeCodePoint x utf16CodeUnits1 utf16CodeUnits2

{-| Single code-unit UTF-16 character -}
{-# INLINABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> Builder
utf16CodeUnits1 unit =
  Builder action 1 1
  where
    action =
      Action $ \array offset -> B.unsafeWrite array offset unit

{-| Double code-unit UTF-16 character -}
{-# INLINABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> Builder
utf16CodeUnits2 unit1 unit2 =
  Builder action 2 1
  where
    action =
      Action $ \array offset -> do
        B.unsafeWrite array offset unit1
        B.unsafeWrite array (succ offset) unit2

{-| Single code-unit UTF-8 character -}
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> Builder
utf8CodeUnits1 unit1 =
  D.utf8CodeUnits1 unit1 utf16CodeUnits1 utf16CodeUnits2

{-| Double code-unit UTF-8 character -}
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> Builder
utf8CodeUnits2 unit1 unit2 =
  D.utf8CodeUnits2 unit1 unit2 utf16CodeUnits1 utf16CodeUnits2

{-| Triple code-unit UTF-8 character -}
{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits3 unit1 unit2 unit3 =
  D.utf8CodeUnits3 unit1 unit2 unit3 utf16CodeUnits1 utf16CodeUnits2

{-| UTF-8 character out of 4 code units -}
{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits4 unit1 unit2 unit3 unit4 =
  D.utf8CodeUnits4 unit1 unit2 unit3 unit4 utf16CodeUnits1 utf16CodeUnits2

{-| ASCII byte string -}
{-# INLINABLE asciiByteString #-}
asciiByteString :: ByteString -> Builder
asciiByteString byteString =
  Builder action length length
  where
    length = ByteString.length byteString
    action =
      Action $ \array -> let
        step byte next index = do
          B.unsafeWrite array index (fromIntegral byte)
          next (succ index)
        in ByteString.foldr step (const (return ())) byteString

{-| Strict text -}
{-# INLINABLE text #-}
text :: Text -> Builder
text text@(C.Text array offset length) =
  Builder action length (Text.length text)
  where
    action =
      Action $ \builderArray builderOffset -> do
        B.copyI builderArray builderOffset array offset (builderOffset + length)

{-| String -}
{-# INLINE string #-}
string :: String -> Builder
string =
  foldMap char

{-| Decimal representation of an integral value -}
{-# INLINABLE decimal #-}
decimal :: Integral a => a -> Builder
decimal i =
  if i >= 0
    then unsignedDecimal i
    else unicodeCodePoint 45 <> unsignedDecimal (negate i)

{-| Decimal representation of an unsigned integral value -}
{-# INLINABLE unsignedDecimal #-}
unsignedDecimal :: Integral a => a -> Builder
unsignedDecimal =
  foldMap decimalDigit . Unfoldr.decimalDigits

{-| Decimal representation of an integral value with thousands separated by the specified character -}
{-# INLINABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: Integral a => Char -> a -> Builder
thousandSeparatedDecimal separatorChar a =
  if a >= 0
    then thousandSeparatedUnsignedDecimal separatorChar a
    else unicodeCodePoint 45 <> thousandSeparatedUnsignedDecimal separatorChar (negate a)

{-| Decimal representation of an unsigned integral value with thousands separated by the specified character -}
{-# INLINABLE thousandSeparatedUnsignedDecimal #-}
thousandSeparatedUnsignedDecimal :: Integral a => Char -> a -> Builder
thousandSeparatedUnsignedDecimal separatorChar a =
  fold $ do
    (index, digit) <- Unfoldr.zipWithReverseIndex $ Unfoldr.decimalDigits a
    if mod index 3 == 0 && index /= 0
      then return (decimalDigit digit <> char separatorChar)
      else return (decimalDigit digit)

{-| Data size in decimal notation over amount of bytes. -}
{-# INLINABLE dataSizeInBytesInDecimal #-}
dataSizeInBytesInDecimal :: Integral a => Char -> a -> Builder
dataSizeInBytesInDecimal separatorChar amount =
  if amount < 1000
    then unsignedDecimal amount <> "B"
    else if amount < 1000000
      then dividedDecimal separatorChar 100 amount <> "kB"
      else if amount < 1000000000
        then dividedDecimal separatorChar 100000 amount <> "MB"
        else if amount < 1000000000000
          then dividedDecimal separatorChar 100000000 amount <> "GB"
        else if amount < 1000000000000000
          then dividedDecimal separatorChar 100000000000 amount <> "TB"
        else if amount < 1000000000000000000
          then dividedDecimal separatorChar 100000000000000 amount <> "PB"
        else if amount < 1000000000000000000000
          then dividedDecimal separatorChar 100000000000000000 amount <> "EB"
          else if amount < 1000000000000000000000000
            then dividedDecimal separatorChar 100000000000000000000 amount <> "ZB"
            else dividedDecimal separatorChar 100000000000000000000000 amount <> "YB"

dividedDecimal :: Integral a => Char -> a -> a -> Builder
dividedDecimal separatorChar divisor n = let
  byDivisor = div n divisor
  byExtraTen = div byDivisor 10
  remainder = byDivisor - byExtraTen * 10
  in if remainder == 0 || byExtraTen >= 10
    then thousandSeparatedDecimal separatorChar byExtraTen
    else thousandSeparatedDecimal separatorChar byExtraTen <> "." <> decimalDigit remainder

{-| Unsigned binary number -}
{-# INLINE unsignedBinary #-}
unsignedBinary :: Integral a => a -> Builder
unsignedBinary =
  foldMap decimalDigit . Unfoldr.binaryDigits

{-| Unsigned binary number -}
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> Builder
unsignedPaddedBinary a =
  padFromLeft (finiteBitSize a) '0' $ foldMap decimalDigit $ Unfoldr.binaryDigits a

{-| Hexadecimal representation of an integral value -}
{-# INLINE hexadecimal #-}
hexadecimal :: Integral a => a -> Builder
hexadecimal i =
  if i >= 0
    then unsignedHexadecimal i
    else unicodeCodePoint 45 <> unsignedHexadecimal (negate i)

{-| Unsigned hexadecimal representation of an integral value -}
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: Integral a => a -> Builder
unsignedHexadecimal =
  foldMap hexadecimalDigit . Unfoldr.hexadecimalDigits

{-| Decimal digit -}
{-# INLINE decimalDigit #-}
decimalDigit :: Integral a => a -> Builder
decimalDigit n =
  unicodeCodePoint (fromIntegral n + 48)

{-| Hexadecimal digit -}
{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Integral a => a -> Builder
hexadecimalDigit n =
  if n <= 9
    then unicodeCodePoint (fromIntegral n + 48)
    else unicodeCodePoint (fromIntegral n + 87)

{-| Intercalate builders -}
{-# INLINE intercalate #-}
intercalate :: Foldable foldable => Builder -> foldable Builder -> Builder
intercalate separator = extract . foldl' step init where
  init = Product2 False mempty
  step (Product2 isNotFirst builder) element = Product2 True $ if isNotFirst
    then builder <> separator <> element
    else element
  extract (Product2 _ builder) = builder

{-| Pad a builder from the left side to the specified length with the specified character -}
{-# INLINABLE padFromLeft #-}
padFromLeft :: Int -> Char -> Builder -> Builder
padFromLeft paddedLength paddingChar builder = let
  builderLength = length builder
  in if paddedLength <= builderLength
    then builder
    else foldMap char (replicate (paddedLength - builderLength) paddingChar) <> builder

{-| Pad a builder from the right side to the specified length with the specified character -}
{-# INLINABLE padFromRight #-}
padFromRight :: Int -> Char -> Builder -> Builder
padFromRight paddedLength paddingChar builder = let
  builderLength = length builder
  in if paddedLength <= builderLength
    then builder
    else builder <> foldMap char (replicate (paddedLength - builderLength) paddingChar)

{-|
Time interval in seconds.
Directly applicable to 'DiffTime' and 'NominalDiffTime'.
-}
{-# INLINABLE intervalInSeconds #-}
intervalInSeconds :: RealFrac seconds => seconds -> Builder
intervalInSeconds interval = flip evalState (round interval) $ do
  seconds <- state (swap . flip divMod 60)
  minutes <- state (swap . flip divMod 60)
  hours <- state (swap . flip divMod 24)
  days <- get
  return $
    padFromLeft 2 '0' (decimal days) <> ":" <>
    padFromLeft 2 '0' (decimal hours) <> ":" <>
    padFromLeft 2 '0' (decimal minutes) <> ":" <>
    padFromLeft 2 '0' (decimal seconds)

{-| Double with a fixed number of decimal places. -}
{-# INLINE fixedDouble #-}
fixedDouble :: Int {-^ Amount of decimals after point. -} -> Double -> Builder
fixedDouble decimalPlaces = fromString . printf ("%." ++ show decimalPlaces ++ "f")

{-| Double multiplied by 100 with a fixed number of decimal places applied and followed by a percent-sign. -}
{-# INLINE doublePercent #-}
doublePercent :: Int {-^ Amount of decimals after point. -} -> Double -> Builder
doublePercent decimalPlaces x = fixedDouble decimalPlaces (x * 100) <> "%"

{-| Hexadecimal readable representation of binary data. -}
{-# INLINE hexData #-}
hexData :: ByteString -> Builder
hexData =
  intercalate " " . fmap mconcat
    . Split.chunksOf 2
    . fmap byte
    . ByteString.unpack
  where
    byte =
      padFromLeft 2 '0' . unsignedHexadecimal
