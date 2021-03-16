module Test.MySolutions where

import Control.Alt
import Prelude

import Control.Monad.State (state)
import Data.Array (foldM, head, nub, scanl, sort, tail)
import Data.Foldable (foldr)
import Data.List (List, List(..), (:))
import Data.Map (Map(..), toUnfoldable, lookup, empty, insert)
import Data.Map (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))

-- Note to reader: Add your solutions to this file
third arr = do 
    skip1 <- tail arr
    skip2 <- tail skip1
    head skip2


possibleSums :: Array Int -> Array Int
possibleSums  = nub <<< sort <<< (foldM (\acc i -> [ acc, acc + i ]) 0)

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM fn (x : xs) = do
  b <- fn x 
  xs' <- filterM fn xs
  pure $ guard b (Cons x Nil) <> xs'
filterM _ Nil = pure Nil

factorDecomposition :: Int -> (List Int)
factorDecomposition n = factorDecomposition' n 2 Nil where 
  factorDecomposition' n p l | (n < p*p)          = l <> pure n
                             | (n `mod` p == 0)   = l <> pure p <> factorDecomposition' (n `div` p) p l
                             | otherwise          = factorDecomposition' n (p+1) l

collapse :: List Int -> List (Tuple Int Int)
collapse = toUnfoldable <<< (foldr qwe empty) where
  qwe v acc = insert v (fromMaybe 0 (lookup v acc)+1) acc
prime_factors :: Int -> String
prime_factors = show <<< factorDecomposition