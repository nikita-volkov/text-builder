module Text.Builder.Action.Definitions
where

import Text.Builder.Prelude
import qualified ChunkTree as A
import qualified Data.Text.Array as B
import qualified Data.Text.Internal as C
import qualified Text.Builder.UTF16 as D


newtype Action =
  Action (forall s. B.MArray s -> Int -> ST s ())

data Builder =
  Builder !Action !Int

instance Monoid Builder where
  mempty =
    Builder (Action (\_ _ -> return ())) 0
  mappend (Builder (Action action1) size1) (Builder (Action action2) size2) =
    Builder action size
    where
      action =
        Action $ \array offset -> do
          action1 array offset
          action2 array (offset + size1)
      size =
        size1 + size2

char :: Char -> Builder
char x =
  charOrd (ord x)

charOrd :: Int -> Builder
charOrd x =
  D.charOrd builder1 builder2 x
  where
    builder1 byte =
      Builder action 1
      where
        action =
          Action $ \array offset -> B.unsafeWrite array offset byte
    builder2 byte1 byte2 =
      Builder action 2
      where
        action =
          Action $ \array offset -> do
            B.unsafeWrite array offset byte1
            B.unsafeWrite array (succ offset) byte2

run :: Builder -> Text
run (Builder (Action action) size) =
  C.text array 0 size
  where
    array =
      runST $ do
        array <- B.new size
        action array 0
        B.unsafeFreeze array
