module Text.Builder.Char.Definitions
where

import Text.Builder.Prelude
import qualified ChunkTree as A


-- |
-- Text builder, which is optimized to be built from individual characters.
newtype Builder =
  Builder (A.ChunkTree Char)
  deriving (Semigroup, Monoid)

{-# INLINE char #-}
char :: Char -> Builder
char x =
  Builder (A.chunk 1 x)
