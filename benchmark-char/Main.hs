module Main where

import Prelude
import Criterion.Main
import qualified TextBuilder as A
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as C
import qualified Data.Text as D


main =
  defaultMain $
  [
    subjectBenchmark "builderSubject" builderSubject
    ,
    subjectBenchmark "lazyTextBuilderSubject" lazyTextBuilderSubject
    ,
    subjectBenchmark "plainTextPackingSubject" plainTextPackingSubject
  ]

subjectBenchmark :: String -> Subject -> Benchmark
subjectBenchmark title subject =
  bgroup title $
  [
    benchmark "Small input" smallInput subject
    ,
    benchmark "Medium input" mediumInput subject
    ,
    benchmark "Large input" largeInput subject
  ]

benchmark :: String -> [Int] -> Subject -> Benchmark
benchmark title input subject =
  bench title $ nf subject $ input

type Subject =
  [Int] -> Text

builderSubject :: Subject
builderSubject =
  A.run . A.string . map chr

lazyTextBuilderSubject :: Subject
lazyTextBuilderSubject =
  C.toStrict . B.toLazyText . B.fromString . map chr

plainTextPackingSubject :: Subject
plainTextPackingSubject =
  D.pack . map chr

{-# NOINLINE smallInput #-}
smallInput :: [Int]
smallInput =
  map ord ['a', 'b', 'Ф', '漢', chr 0x11000]

{-# NOINLINE mediumInput #-}
mediumInput :: [Int]
mediumInput =
  mconcat (replicate 1000 smallInput)

{-# NOINLINE largeInput #-}
largeInput :: [Int]
largeInput =
  mconcat (replicate 100000 smallInput)

