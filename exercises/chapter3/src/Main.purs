module Main where

import Data.Lazy
import Prelude

import Data.List (List(..), range)
import Effect (Effect)
import Effect.Console (log, logShow)


main :: Effect Unit
main = do
  logShow $ (range 0 15 )
