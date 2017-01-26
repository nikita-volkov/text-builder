module Text.Builder.Action.Definitions
where

import Text.Builder.Prelude
import qualified ChunkTree as A
import qualified Data.Text.Array as B
import qualified Data.Text.Internal as C


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
  if x < 0x10000
    then
      Builder action1 1
    else
      Builder action2 2
  where
    action1 =
      Action $ \array offset -> B.unsafeWrite array offset (fromIntegral x)
    action2 =
      Action $ \array offset -> do
        B.unsafeWrite array offset byte1
        B.unsafeWrite array (succ offset) byte2
      where
        m =
          x - 0x10000
        byte1 =
          fromIntegral (shiftR m 10 + 0xD800)
        byte2 =
          fromIntegral ((m .&. 0x3FF) + 0xDC00)

run :: Builder -> Text
run (Builder (Action action) size) =
  C.text array 0 size
  where
    array =
      runST $ do
        array <- B.new size
        action array 0
        B.unsafeFreeze array
