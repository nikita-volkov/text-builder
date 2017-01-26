module Main where

import Prelude
import Criterion.Main
import qualified Text.Builder as A


main =
  defaultMain $
  [
    benchmark "Small input" smallSample builderSubject
    ,
    benchmark "Large input" largeSample builderSubject
  ]

benchmark :: String -> Sample -> Subject -> Benchmark
benchmark title sample subject =
  bench title $ nf sample $ subject

data Subject =
  forall a. Subject (Char -> a) (a -> a -> a) a (a -> Text)

type Sample =
  Subject -> Text

builderSubject :: Subject
builderSubject =
  Subject A.char mappend mempty A.run

{-# NOINLINE smallSample #-}
smallSample :: Sample
smallSample (Subject char (<>) mempty run) =
  run $
  (char 'a' <> char 'b') <>
  char 'Ф' <>
  (char '漢' <> char (chr 0x11000))

{-# NOINLINE largeSample #-}
largeSample :: Sample
largeSample (Subject char (<>) mempty run) =
  run $
  foldl' (<>) mempty $ replicate 100000 $
  (char 'a' <> char 'b') <>
  char 'Ф' <>
  (char '漢' <> char (chr 0x11000))

