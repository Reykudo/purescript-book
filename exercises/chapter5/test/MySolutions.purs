module Test.MySolutions where

import Data.Person
import Data.Picture
import Prelude

import Data.Maybe (Maybe(..))
import Math (pi)

-- factorial 0 = 1
factorial n | n >= 0 = factorial' n 1 where
    factorial' 0 acc = acc
    factorial' n acc = factorial' (n-1) (n * acc)
factorial _ = 0
-- binomial 
binomial n k | n >= k = (factorial n) / ((factorial k) * (factorial (n - k)))
             | otherwise = 0

pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n-1) k + (pascal (n-1) (k-1))



sameCity { address: {city: a} } { address: {city: b} }  | a == b = true
                                                        | otherwise = false

fromSingleton _ [b] = b
fromSingleton a _ = a

circleAtOrigin = Circle origin 10.0




centerShape :: Shape -> Shape
centerShape (Circle c r) = Circle origin r
centerShape (Rectangle c w h) = Rectangle origin w h
centerShape line@(Line (Point s) (Point e)) =
  (Line
    (Point { x: s.x - deltaX, y: s.y - deltaY })
    (Point { x: e.x - deltaX, y: e.y - deltaY })
  )
  where
  delta = getCenter line
  deltaX = getX delta
  deltaY = getY delta
centerShape (Text loc text) = Text origin text

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r * i)
scaleShape i (Rectangle c w h) = Rectangle c (w * i) (h * i)
scaleShape i (Line (Point s) (Point e)) =
  (Line
    (Point { x: s.x * i, y: s.y * i })
    (Point { x: e.x * i, y: e.y * i })
  )
scaleShape i text = text


doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText (Text _ str) = Just str
shapeText _ = Nothing

area (Circle _ r) = pi*r*r
area (Rectangle _ p1 p2) = p1*p2 
area (Text _ _ ) = 0.0
area (Line _ _) = 0.0

-- instance showBoolean :: Show Boolean where
--   show true = "true"
--   show false = "false"