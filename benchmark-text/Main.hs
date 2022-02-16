module Main where

import Criterion.Main
import qualified Data.Text as D
import qualified Data.Text.Lazy as C
import qualified Data.Text.Lazy.Builder as B
import qualified Text.Builder as A
import Prelude

main =
  defaultMain $
    [ subjectBenchmark "builderSubject" builderSubject,
      subjectBenchmark "lazyTextBuilderSubject" lazyTextBuilderSubject
    ]

subjectBenchmark :: String -> Subject -> Benchmark
subjectBenchmark title subject =
  bgroup title $
    [ benchmark "Small input" smallSample subject,
      benchmark "Large input" largeSample subject
    ]

benchmark :: String -> Sample -> Subject -> Benchmark
benchmark title sample subject =
  bench title $ nf sample $ subject

data Subject
  = forall a. Subject (Text -> a) (a -> a -> a) a (a -> Text)

type Sample =
  Subject -> Text

builderSubject :: Subject
builderSubject =
  Subject A.text mappend mempty A.run

lazyTextBuilderSubject :: Subject
lazyTextBuilderSubject =
  Subject B.fromText mappend mempty (C.toStrict . B.toLazyText)

{-# NOINLINE smallSample #-}
smallSample :: Sample
smallSample (Subject text (<>) mempty run) =
  run $
    text "abcd" <> (text "ABCD" <> text "Фываолдж") <> text "漢"

{-# NOINLINE largeSample #-}
largeSample :: Sample
largeSample (Subject text (<>) mempty run) =
  run $
    foldl' (<>) mempty $
      replicate 100000 $
        text "abcd" <> (text "ABCD" <> text "Фываолдж") <> text "漢"
