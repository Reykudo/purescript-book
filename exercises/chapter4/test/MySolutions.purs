module Test.MySolutions where

import Data.Array
import Math
import Prelude

import Control.MonadZero (guard)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class.Console (log, logShow)
import Test.Examples (factors)
import Test.NoPeeking.Solutions (allTrue, isPrime, triples)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven v = v `mod` 2 == 0

-- countEven arr = case head arr of
--                 Nothing -> 0
--                 Just v -> (incIf <<< isEven) v + (restArr <<< tail) arr
--                     where
--                       incIf bool = if bool
--                                      then 1
--                                      else 0
--                       restArr a = case a of
--                                     Nothing -> 0
--                                     Just q -> countEven q
countEven = length <<< filter isEven

-- 
isPrime n = n > 1 && 1 == (length <<< factors) n

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
  first <- arr1
  second <- arr2
  pure [ first, second ]
triples :: Int -> Array (Array Int)
triples n =  do
      i <- 1..n
      j <- i..n
      g <- j..n
      guard $ i*i + j*j == g*g
      [[i,j, g]]

allTrue = foldl (&&) true

fibTailRec n = fib' (1..n) [] where
  fib' iter res = fib' (fromMaybe [] (tail iter)) 