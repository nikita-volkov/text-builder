{-# OPTIONS_GHC -Wno-orphans #-}

module Util.ExtraInstances where

import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary TextLazyBuilder.Builder where
  arbitrary =
    TextLazyBuilder.fromLazyText <$> arbitrary
