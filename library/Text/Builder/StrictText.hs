module Text.Builder.StrictText
where

import Text.Builder.Prelude
import Data.Text
import qualified ChunkTree as A
import qualified Text.Builder.Char.Definitions as B


{-|
Construct text from a character-specialised builder.
-}
charBuilder :: B.Builder -> Text
charBuilder (B.Builder chunkTree) =
  pack (toList chunkTree)
