module Text.Builder
(
  Builder,
  run,
  char,
  text,
  string,
  utf16CodeUnits1,
  utf16CodeUnits2,
  utf8CodeUnits1,
  utf8CodeUnits2,
  utf8CodeUnits3,
  utf8CodeUnits4,
)
where

import Text.Builder.Prelude
import qualified Data.Text.Array as B
import qualified Data.Text.Internal as C
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

instance Semigroup Builder

{-# INLINE char #-}
char :: Char -> Builder
char x =
  unicodeCodePoint (ord x)

{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> Builder
unicodeCodePoint =
  D.unicodeCodePoint utf16CodeUnits1 utf16CodeUnits2

{-# INLINABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> Builder
utf16CodeUnits1 byte =
  Builder action 1
  where
    action =
      Action $ \array offset -> B.unsafeWrite array offset byte

{-# INLINABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> Builder
utf16CodeUnits2 byte1 byte2 =
  Builder action 2
  where
    action =
      Action $ \array offset -> do
        B.unsafeWrite array offset byte1
        B.unsafeWrite array (succ offset) byte2

{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> Builder
utf8CodeUnits1 =
  D.utf8CodeUnits1 utf16CodeUnits1 utf16CodeUnits2

{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> Builder
utf8CodeUnits2 =
  D.utf8CodeUnits2 utf16CodeUnits1 utf16CodeUnits2

{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits3 =
  D.utf8CodeUnits3 utf16CodeUnits1 utf16CodeUnits2

{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
utf8CodeUnits4 =
  D.utf8CodeUnits4 utf16CodeUnits1 utf16CodeUnits2

{-# INLINABLE text #-}
text :: Text -> Builder
text (C.Text array offset length) =
  Builder action actualLength
  where
    action =
      Action $ \builderArray builderOffset -> do
        B.copyI builderArray builderOffset array offset (builderOffset + actualLength)
    actualLength =
      length - offset

{-# INLINE string #-}
string :: String -> Builder
string =
  foldMap char

run :: Builder -> Text
run (Builder (Action action) size) =
  C.text array 0 size
  where
    array =
      runST $ do
        array <- B.new size
        action array 0
        B.unsafeFreeze array
