module Test.MySolutions where

import Data.Show
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, wrap)

-- Note to reader: Add your solutions to this file
data Point
  = Point
    { x :: Number
    , y :: Number
    }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = (show real) <> (if (imaginary >= 0.0) then "+" else "") <> (show imaginary) <> "i"

derive instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  add (Complex { real: x, imaginary: y }) (Complex { real: u, imaginary: v }) = (Complex { real: x + u, imaginary: y + v })
  zero = wrap zero
  mul (Complex { real: x, imaginary: y }) (Complex { real: u, imaginary: v }) 
    = (Complex { real: (x * u - y * v), imaginary: (x * v + y * u) })
  one = wrap one


derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a1 a2) (NonEmpty b1 b2) = a1 == b1 && a2 == b2