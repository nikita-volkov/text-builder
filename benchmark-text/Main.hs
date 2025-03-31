module Main where

import Criterion.Main
import qualified Data.Text.Lazy as C
import qualified Data.Text.Lazy.Builder as B
import qualified TextBuilder as A
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup
        "< 1KB"
        [ bench "TextBuilder" (nf (simulate 20) A.run),
          bench "Data.Text.Lazy.Builder" (nf (simulate 20) (C.toStrict . B.toLazyText))
        ],
      bgroup
        "< 1MB"
        [ bench "TextBuilder" (nf (simulate 20_000) A.run),
          bench "Data.Text.Lazy.Builder" (nf (simulate 20_000) (C.toStrict . B.toLazyText))
        ],
      bgroup
        "< 1GB"
        [ bench "TextBuilder" (nf (simulate 20_000_000) A.run),
          bench "Data.Text.Lazy.Builder" (nf (simulate 20_000_000) (C.toStrict . B.toLazyText))
        ]
    ]

{-# NOINLINE simulate #-}
simulate :: (Monoid a, IsString a) => Int -> (a -> Text) -> Text
simulate repetitions compile =
  ("abcd" <> ("ABCD" <> "Фываолдж") <> "漢")
    & replicate repetitions
    & mconcat
    & compile
