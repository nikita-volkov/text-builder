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
        "Left-biased mappend"
        let {-# NOINLINE sampleBySize #-}
            sampleBySize :: (Monoid a, IsString a) => Int -> (a -> Text) -> Text
            sampleBySize size compile =
              "фывапролдж"
                & replicate size
                & foldl' (<>) mempty
                & compile
         in [ bgroup
                "1kB"
                let sample = sampleBySize 100
                 in [ bench "TextBuilder" (nf sample A.run),
                      bench "Data.Text.Lazy.Builder" (nf sample (C.toStrict . B.toLazyText))
                    ],
              bgroup
                "1MB"
                let sample = sampleBySize 100_000
                 in [ bench "TextBuilder" (nf sample A.run),
                      bench "Data.Text.Lazy.Builder" (nf sample (C.toStrict . B.toLazyText))
                    ]
            ],
      bgroup
        "Right-biased mappend"
        let {-# NOINLINE sampleBySize #-}
            sampleBySize :: (Monoid a, IsString a) => Int -> (a -> Text) -> Text
            sampleBySize size compile =
              "фывапролдж"
                & replicate size
                & foldl' (flip (<>)) mempty
                & compile
         in [ bgroup
                "1kB"
                let sample = sampleBySize 100
                 in [ bench "TextBuilder" (nf sample A.run),
                      bench "Data.Text.Lazy.Builder" (nf sample (C.toStrict . B.toLazyText))
                    ],
              bgroup
                "1MB"
                let sample = sampleBySize 100_000
                 in [ bench "TextBuilder" (nf sample A.run),
                      bench "Data.Text.Lazy.Builder" (nf sample (C.toStrict . B.toLazyText))
                    ]
            ],
      bgroup
        "mconcat"
        let {-# NOINLINE sampleBySize #-}
            sampleBySize :: (Monoid a, IsString a) => Int -> (a -> Text) -> Text
            sampleBySize size compile =
              "фывапролдж"
                & replicate size
                & mconcat
                & compile
         in [ bgroup
                "1kB"
                let sample = sampleBySize 100
                 in [ bench "TextBuilder" (nf sample A.run),
                      bench "Data.Text.Lazy.Builder" (nf sample (C.toStrict . B.toLazyText))
                    ],
              bgroup
                "1MB"
                let sample = sampleBySize 100_000
                 in [ bench "TextBuilder" (nf sample A.run),
                      bench "Data.Text.Lazy.Builder" (nf sample (C.toStrict . B.toLazyText))
                    ]
            ]
    ]
