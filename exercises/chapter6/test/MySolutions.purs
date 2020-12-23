module Test.MySolutions where

import Data.Foldable
import Data.Show
import Data.Show
import Prelude
import Prelude

import Data.Array (concat, cons, length, nub, nubBy, nubByEq, nubEq)
import Data.Functor as Functor
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Hashable (class Hashable, hashEqual, hash)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
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

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [ e2 ] <> a2)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> " " <> show a1

instance functorNonEmpty :: Functor NonEmpty where
  map fn (NonEmpty single array) = NonEmpty (fn single) (Functor.map fn array)

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b

nonEmptyToArray (NonEmpty a b) = [a] <> b

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr fn start = (foldr fn start) <<< nonEmptyToArray
  foldl fn start = (foldl fn start) <<< nonEmptyToArray
  foldMap fn = (foldMap fn) <<< nonEmptyToArray


data OneMore f a = OneMore a (f a)
instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr fn start (OneMore a b) = fn a (foldr fn start b)
  foldl fn start (OneMore a b) = foldl fn (fn start a) b
  foldMap fn (OneMore a b) = fn a <> (foldMap fn b)


derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape
derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = case maximum arr of
  Just m -> m

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply m) a = m*a

instance actionMultiplyString :: Action Multiply String where
  act (Multiply m) a = power a m

instance actionArray :: Action m a => Action m (Array a) where
  act ac ar = map (act ac) ar

newtype Self m = Self m
derive newtype instance selfEq :: Eq a => Eq (Self a)
derive newtype instance selfShow :: Show a => Show (Self a)

derive newtype instance multiplyEq :: Eq (Multiply)
derive newtype instance multiplyShow :: Show (Multiply)

instance actSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates a1 = (length (nubByEq (\a b -> hashEqual a b && a == b ) a1)) /= (length a1)

instance hashableHour :: Hashable Hour where
  hash (Hour h) = hash (h `mod` 12)