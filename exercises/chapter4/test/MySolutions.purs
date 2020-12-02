module Test.MySolutions where

import Data.Array
import Data.Maybe
import Prelude

import Control.MonadZero (guard)
import Data.Foldable (foldl)
import Data.Path (Path(..), filename, isDirectory, ls)
import Data.String (split)
import Data.String.Pattern
import Test.Examples (allFiles')

-- Note to reader: Add your solutions to this file
reverse = foldl (flip cons) []


onlyFiles :: Path -> Array Path

onlyFiles = filter (not isDirectory) <<< allFiles'


-- whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ whereIs' $ allFiles' path
  where
--   whereIs' :: Array Path -> Array Path
  whereIs' paths = do
    path' <- paths
    child <- ls path'
    guard $ fileName == (fromMaybe "" (last $ (split (Pattern "/")) (filename child)))
    pure path'
