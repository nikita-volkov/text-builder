module Text.Builder
(
  Builder,
  run,
  char,
)
where

import Text.Builder.Prelude
import qualified ChunkTree as A
import qualified Data.Text.Array as B
import qualified Data.Text.Internal as C
import qualified Text.Builder.UTF16 as D


newtype Builder =
  Builder (A.ChunkTree (B.Array, Int))
  deriving (Monoid, Semigroup)

{-# INLINE char #-}
char :: Char -> Builder
char x =
  charOrd (ord x)

{-# INLINABLE charOrd #-}
charOrd :: Int -> Builder
charOrd x =
  D.charOrd builder1 builder2 x
  where
    builder1 byte =
      Builder (A.chunk 1 (array, 1))
      where
        array =
          runST $ do
            array <- B.new 1
            B.unsafeWrite array 0 byte
            B.unsafeFreeze array
    builder2 byte1 byte2 =
      Builder (A.chunk 2 (array, 2))
      where
        array =
          runST $ do
            array <- B.new 2
            B.unsafeWrite array 0 byte1
            B.unsafeWrite array 1 byte2
            B.unsafeFreeze array

run :: Builder -> Text
run (Builder chunkTree) =
  C.text array 0 size
  where
    size =
      A.length chunkTree
    array =
      runST $ do
        array <- B.new size
        foldlM (step array) 0 chunkTree
        B.unsafeFreeze array
      where
        step array i (bytes, bytesSize) =
          B.copyI array i bytes 0 nextI $> nextI
          where
            nextI =
              i + bytesSize
