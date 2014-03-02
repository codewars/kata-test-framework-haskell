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
import Test.Hspec.Runner (hspecWithFormatter)
import Test.Hspec.Formatters (silent,
                              Formatter (..),
                              writeLine,
                              getRealTime,
                              getFailCount,
                              formatException)
import Test.QuickCheck hiding (reason)
import Control.Exception (evaluate)
import Test.HUnit (assertFailure)
import Control.Monad (unless)
import Text.Printf (printf)
import Data.Aeson.Encode (encode)
import Data.ByteString.Lazy.Char8 (unpack)

codewarsFormatter :: Formatter
codewarsFormatter =
  silent {
    headerFormatter = do
       writeLine "{"
       writeLine $ quote "output" ++ " : { "
       
 , exampleGroupStarted = \_ nesting name ->
     writeLine $
     indentationFor nesting ++ quote name ++ " : { "

 , exampleSucceeded = \(nesting, requirement) ->
     writeLine $
     indentationFor nesting
     ++ quote requirement ++ " : { \"success\": true },"

 , exampleFailed = \(nesting, requirement) reason ->
     writeLine $
     indentationFor nesting
     ++ quote requirement ++ " : { \"success\": false,"
     ++ " \"reason\": " ++ err reason ++ " },"
     
 , exampleGroupDone = writeLine "   },"
 
 , footerFormatter = do
       writeLine "},"
       n <- getFailCount
       writeLine $ quote "success"
         ++ ": " ++ if n == 0 then "true" else "false" ++ ","
       time <- getRealTime
       writeLine (printf "\"time\": %f" time)
       writeLine "}"
    }
  where
    quote = unpack . encode
    err reason = quote $ either (("uncaught exception: " ++) . formatException) id reason
    indentationFor nesting = replicate ((1 + length nesting) * 2) ' '


spec :: Spec -> IO ()
spec = hspecWithFormatter codewarsFormatter

infix 1 `shouldNotBe`
shouldNotBe :: (Eq a, Show a) => a -> a -> Expectation
shouldNotBe expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = "expected: " ++ show expected
              ++ "\n to be different than: " ++ show actual

