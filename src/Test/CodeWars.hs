{-# LANGUAGE NoMonomorphismRestriction #-}
module Test.CodeWars (
  -- HSpec
  spec
  , Spec
  , describe
  , it

  -- QuickCheck
  , property
  -- Exceptions
  , evaluate

  -- Expectation Combinators,
  -- see https://github.com/sol/hspec-expectations/blob/master/src/Test/Hspec/Expectations.hs
  , Expectation
  , expectationFailure
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  , shouldContain
  , shouldMatchList
  , shouldReturn
  , shouldThrow
  , Selector
  , anyException
  , anyErrorCall
  , anyIOException
  , anyArithException
  , errorCall
                     ) where

import Test.Hspec
import Test.Hspec.Runner (hspecWith, defaultConfig, Config (configFormatter,configHandle))
import Test.QuickCheck hiding (reason)
import Control.Exception (evaluate)
import Test.HUnit (assertFailure)
import Control.Monad (unless)
import Data.ByteString.Char8 (pack, unpack)
import System.Environment (withArgs)
import qualified Data.Knob 
import System.IO (hClose, IOMode( WriteMode ))
import Test.CodeWars.Formatter (json)
import Data.String.Utils (replace)

spec :: Spec -> IO String
spec s = withArgs [] $ do
  knob <- Data.Knob.newKnob (pack [])
  h <- Data.Knob.newFileHandle knob "Test.CodeWars" WriteMode
  _ <- hspecWith defaultConfig {configFormatter = json
                               , configHandle = Left h} s
  hClose h
  bytes <- Data.Knob.getContents knob
  let out = replace ",}" "}" $ unpack bytes
  return out

infix 1 `shouldNotBe`
shouldNotBe :: (Eq a, Show a) => a -> a -> Expectation
shouldNotBe expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = "expected: " ++ show expected
              ++ "\n to be different than: " ++ show actual

