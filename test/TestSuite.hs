module Main where

import Test.CodeWars
import Hamming (hammings)
import System.IO.Unsafe (unsafePerformIO)
import Data.JSON2.Parser (parseJson)
--import Data.JSON2 (pprint)

fooPrinter :: String
fooPrinter = unsafePerformIO $ do
  putStr "Foo"
  return "Bar"

failingTest :: Spec
failingTest = 
  describe "Failing test" $
    it "declares something baldly False" $
      1 `shouldBe` (2 :: Int)

bigTest :: Spec
bigTest =  do
  describe "Prelude.head" $ do 
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)
      
    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)
  
    it "throws an exception if used with an empty list" $
      evaluate (head []) `shouldThrow` anyException

  describe "hammings" $ do
    it "starts with [1,2,3,4,5]" $
      take 5 hammings `shouldBe` [1,2,3,4,5]

  describe "fooPrinter" $ do
    it "is pretending to be the pure value \"Bar\"" $
      fooPrinter `shouldBe` "Bar"

isCorrectlyFormattedJSON :: String -> Bool
isCorrectlyFormattedJSON s =
  either (const False) (const True) (parseJson s)

jsonTest :: IO Spec
jsonTest = do

  out <- spec $ describe "Test" $ it "should always be True" $ True `shouldBe` True

  return $ do
    describe "JSON Controls" $ do
      describe "1" $
        it "is a valid JSON" $
          "1" `shouldSatisfy` isCorrectlyFormattedJSON
    
    describe "Test Output" $ do
      it "is a valid JSON" $
        out `shouldSatisfy` isCorrectlyFormattedJSON

main :: IO ()
main = do
  jt <- jsonTest
  out <- spec $ do {bigTest ; jt}
  putStrLn out
