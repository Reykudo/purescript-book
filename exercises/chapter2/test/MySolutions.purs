module Test.MySolutions where

import Prelude

import Control.Monad.List.Trans (mapMaybe)
import Data.Int (fromString, radix, toNumber)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Maybe as Maybe
import Math (e, pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal l w = sqrt (l * l + w * w)

circleArea radius = radius * radius * pi
addE :: String -> Number

addE value = e + (toNumber (mapMaybe (fromString value)))