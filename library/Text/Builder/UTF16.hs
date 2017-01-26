module Text.Builder.UTF16
where

import Text.Builder.Prelude
import qualified Data.Text.Array as B


{-# INLINE char #-}
char :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Char -> x
char cont1 cont2 x =
  charOrd cont1 cont2 (ord x)

{-# INLINE charOrd #-}
charOrd :: (Word16 -> x) -> (Word16 -> Word16 -> x) -> Int -> x
charOrd cont1 cont2 x =
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
