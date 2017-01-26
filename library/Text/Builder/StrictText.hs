module Text.Builder.StrictText
where

import Text.Builder.Prelude
import Data.Text
import qualified ChunkTree as A
import qualified Text.Builder.Char.Definitions as B


fromCharBuilder :: B.Builder -> Text
fromCharBuilder (B.Builder chunkTree) =
  pack (toList chunkTree)
