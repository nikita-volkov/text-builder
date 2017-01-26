module Text.Builder.Char.Definitions
where

import Text.Builder.Prelude
import qualified ChunkTree as A
import qualified Data.Text.Array as B
import qualified Data.Text.Internal as C
import qualified Text.Builder.UTF16 as D


-- |
-- Text builder, which is optimized to be built from individual characters.
newtype Builder =
  Builder (A.ChunkTree Word16)
  deriving (Semigroup, Monoid)

{-# INLINE char #-}
char :: Char -> Builder
char =
  D.char builder1 builder2
  where
    builder1 byte =
      Builder (A.chunk 1 byte)
    builder2 byte1 byte2 =
      Builder (A.chunk 1 byte1 <> A.chunk 1 byte2)

run :: Builder -> Text
run (Builder chunkTree) =
  C.text array 0 size
  where
    size =
      A.length chunkTree
    array =
      runST $ do
        array <- B.new size
        foldlM (\i byte -> B.unsafeWrite array i byte $> succ i) 0 chunkTree
        B.unsafeFreeze array
