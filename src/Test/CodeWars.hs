module Test.CodeWars (
  hspec
  , describe
  , it
  , property
  , evaluate
  , anyException
  , shouldBe
  , shouldThrow
                     ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
