module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either)
import Data.Maybe (Maybe(..))

-- Note to reader: Add your solutions to this file
addMaybe = lift2 (+)
subMaybe = lift2 (-)
mulMaybe = lift2 (*)
divMaybe = lift2 (/)

addApply = lift2 (+)
subApply = lift2 (-)
mulApply = lift2 (*)
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)

combineMaybe (Just x) = map Just x
combineMaybe _ = pure Nothing
