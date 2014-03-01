module Main where

import Test.CodeWars
import Hamming (hammings)


main :: IO ()
main = hspec $ do
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

