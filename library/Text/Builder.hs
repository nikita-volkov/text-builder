module Text.Builder
(
  Builder,
  run,
  length,
  null,
  intercalate,
  char,
  text,
  string,
  unicodeCodePoint,
  utf16CodeUnits1,
  utf16CodeUnits2,
  utf8CodeUnits1,
  utf8CodeUnits2,
  utf8CodeUnits3,
  utf8CodeUnits4,
  decimal,
  unsignedDecimal,
  decimalDigit,
  hexadecimal,
  unsignedHexadecimal,
  hexadecimalDigit,
)
where

import Text.Builder.Prelude hiding (length, null, intercalate)
import qualified Data.Text.Array as B
import qualified Data.Text.Internal as C
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Text.Builder.UTF16 as D


newtype Action =
  Action (forall s. B.MArray s -> Int -> ST s ())

data Builder =
  Builder !Action !Int

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Builder (Action (\_ _ -> return ())) 0
  {-# INLINABLE mappend #-}
  mappend (Builder (Action action1) size1) (Builder (Action action2) size2) =
    Builder action size
    where
      action =
        Action $ \array offset -> do
          action1 array offset
          action2 array (offset + size1)
      size =
        size1 + size2

instance Semigroup Builder where
  (<>) = mappend

instance IsString Builder where
  fromString = string

{-# INLINE char #-}
char :: Char -> Builder
char x =
  unicodeCodePoint (ord x)

{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> Builder
unicodeCodePoint x =
  D.unicodeCodePoint x utf16CodeUnits1 utf16CodeUnits2

{-# INLINABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> Builder
utf16CodeUnits1 unit =
  Builder action 1
  where
    action =
      Action $ \array offset -> B.unsafeWrite array offset unit

{-# INLINABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> Builder
utf16CodeUnits2 unit1 unit2 =
  Builder action 2
  where
    action =
      Action $ \array offset -> do
        B.unsafeWrite array offset unit1
        B.unsafeWrite array (succ offset) unit2

{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> Builder
utf8CodeUnits1 unit1 =
  D.utf8CodeUnits1 unit1 utf16CodeUnits1 utf16CodeUnits2

{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> Builder
utf8CodeUnits2 unit1 unit2 =
  D.utf8CodeUnits2 unit1 unit2 utf16CodeUnits1 utf16CodeUnits2

{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits3 unit1 unit2 unit3 =
  D.utf8CodeUnits3 unit1 unit2 unit3 utf16CodeUnits1 utf16CodeUnits2

{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits4 unit1 unit2 unit3 unit4 =
  D.utf8CodeUnits4 unit1 unit2 unit3 unit4 utf16CodeUnits1 utf16CodeUnits2

{-# INLINABLE text #-}
text :: Text -> Builder
text (C.Text array offset length) =
  Builder action length
  where
    action =
      Action $ \builderArray builderOffset -> do
        B.copyI builderArray builderOffset array offset (builderOffset + length)

{-# INLINE string #-}
string :: String -> Builder
string =
  foldMap char

{-# INLINABLE decimal #-}
decimal :: Integral a => a -> Builder
decimal i =
  if i >= 0
    then unsignedDecimal i
    else unicodeCodePoint 45 <> unsignedDecimal (negate i)

{-# INLINABLE unsignedDecimal #-}
unsignedDecimal :: Integral a => a -> Builder
unsignedDecimal =
  loop mempty
  where
    loop builder !i =
      case quotRem i 10 of
        (quot, !rem) ->
          case hexadecimalDigit rem <> builder of
            newBuilder ->
              if quot /= 0
                then loop newBuilder quot
                else newBuilder

{-# INLINE hexadecimal #-}
hexadecimal :: Integral a => a -> Builder
hexadecimal i =
  if i >= 0
    then unsignedHexadecimal i
    else unicodeCodePoint 45 <> unsignedHexadecimal (negate i)

{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: Integral a => a -> Builder
unsignedHexadecimal =
  loop mempty
  where
    loop builder !i =
      case quotRem i 16 of
        (quot, !rem) ->
          case hexadecimalDigit rem <> builder of
            newBuilder ->
              if quot /= 0
                then loop newBuilder quot
                else newBuilder

{-# INLINE decimalDigit #-}
decimalDigit :: Integral a => a -> Builder
decimalDigit n =
  unicodeCodePoint (fromIntegral n + 48)

{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Integral a => a -> Builder
hexadecimalDigit n =
  if n <= 9
    then unicodeCodePoint (fromIntegral n + 48)
    else unicodeCodePoint (fromIntegral n + 87)

{-# INLINE length #-}
length :: Builder -> Int
length (Builder _ x) = x

{-# INLINE null #-}
null :: Builder -> Bool
null = (== 0) . length

{-# INLINE intercalate #-}
intercalate :: Foldable foldable => Builder -> foldable Builder -> Builder
intercalate separator = extract . foldl' step init where
  init = Product2 False mempty
  step (Product2 isNotFirst builder) element = Product2 True $ if isNotFirst
    then builder <> separator <> element
    else element
  extract (Product2 _ builder) = builder

run :: Builder -> Text
run (Builder (Action action) size) =
  C.text array 0 size
  where
    array =
      runST $ do
        array <- B.new size
        action array 0
        B.unsafeFreeze array
