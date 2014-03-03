module Main where

import Test.CodeWars
import Hamming (hammings)
import System.IO.Unsafe (unsafePerformIO)

fooPrinter :: String
fooPrinter = unsafePerformIO $ do
  putStr "Foo"
  return "Bar"

failingTest :: Spec
failingTest = 
  describe "Failing test" $
    it "declares something baldly False" $
      1 `shouldBe` (2 :: Int)

{- do
        

    
-}

main :: IO ()
main = do
  out <- spec $ do

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

    failingTest
                               
  putStrLn out
  
  -- TODO: test that stuff is correctly formatted JSON, since it's a bit hacked right now
