module Hamming (
  hammings
               ) where

hammings :: [Integer]
hammings = 1 : weave hammings hammings hammings
  where
    weave (x2:x2s) (x3:x3s) (x5:x5s) = minX : weave x2s' x3s' x5s'
      where
        (x2',x3',x5') = (2*x2,3*x3,5*x5)
        minX = minimum [x2', x3', x5']
        x2s' = if x2' <= minX then x2s else x2:x2s
        x3s' = if x3' <= minX then x3s else x3:x3s
        x5s' = if x5' <= minX then x5s else x5:x5s
