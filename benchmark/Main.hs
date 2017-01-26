module Main where

import Prelude
import Criterion.Main
import qualified Text.Builder.Char as A
import qualified Text.Builder.StrictText as B
import qualified Text.Builder.Action as C


main =
  defaultMain $
  [
    benchmark "Small input / charBuilder" smallSample charBuilderSubject
    ,
    benchmark "Small input / actionBuilder" smallSample actionBuilderSubject
    ,
    benchmark "Large input / charBuilder" largeSample charBuilderSubject
    ,
    benchmark "Large input / actionBuilder" largeSample actionBuilderSubject
  ]

benchmark :: String -> Sample -> Subject -> Benchmark
benchmark title sample subject =
  bench title $ nf sample $ subject

data Subject =
  forall a. Subject (Char -> a) (a -> a -> a) a (a -> Text)

type Sample =
  Subject -> Text

charBuilderSubject :: Subject
charBuilderSubject =
  Subject A.char mappend mempty B.charBuilder

actionBuilderSubject :: Subject
actionBuilderSubject =
  Subject C.char mappend mempty B.actionBuilder

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

