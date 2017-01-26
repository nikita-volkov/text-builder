module Text.Builder.UTF16
where

import Text.Builder.Prelude


{-# INLINE char #-}
char :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Char -> x
char cont1 cont2 x =
  unicodeCodePoint cont1 cont2 (ord x)

{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Int -> x
unicodeCodePoint cont1 cont2 x =
  if x < 0x10000
    then cont1 (fromIntegral x)
    else cont2 cont2Byte1 cont2Byte2
  where
    m =
      x - 0x10000
    cont2Byte1 =
      fromIntegral (shiftR m 10 + 0xD800)
    cont2Byte2 =
      fromIntegral ((m .&. 0x3FF) + 0xDC00)

{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Word8 -> x
utf8CodeUnits1 cont1 _ x =
  cont1 (fromIntegral x)

{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Word8 -> Word8 -> x
utf8CodeUnits2 cont1 _ byte1 byte2 =
  cont1 (shiftL (fromIntegral byte1 - 0xC0) 6 + fromIntegral byte2 - 0x80)

{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Word8 -> Word8 -> Word8 -> x
utf8CodeUnits3 cont1 cont2 byte1 byte2 byte3 =
  unicodeCodePoint cont1 cont2 unicode
  where
    unicode =
      shiftL (fromIntegral byte1 - 0xE0) 12 +
      shiftL (fromIntegral byte2 - 0x80) 6 +
      fromIntegral byte3 - 0x80

{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Word8 -> Word8 -> Word8 -> Word8 -> x
utf8CodeUnits4 cont1 cont2 byte1 byte2 byte3 byte4 =
  unicodeCodePoint cont1 cont2 unicode
  where
    unicode =
      shiftL (fromIntegral byte1 - 0xE0) 18 +
      shiftL (fromIntegral byte2 - 0x80) 12 +
      shiftL (fromIntegral byte3 - 0x80) 6 +
      fromIntegral byte4 - 0x80
