module Test.MySolutions where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.String (length)
import Data.Traversable (traverse)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

-- Note to reader: Add your solutions to this file
concatenateFiles :: String -> String -> String -> Aff Unit
concatenateFiles firstPath secondPath outPath = do
  f1 <- readTextFile UTF8 firstPath
  f2 <- readTextFile UTF8 secondPath
  writeTextFile UTF8 outPath (f1 <> f2)

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany files out = do
  l <- traverse (readTextFile UTF8) files
  writeTextFile UTF8 out $ fold l

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters path =
  attempt do
    s <- (readTextFile UTF8 path)
    pure (length s)

writeGet :: String -> FilePath -> Aff Unit
writeGet url path = do
  response <- AX.get ResponseFormat.string url
  case response of
    Right res -> writeTextFile UTF8 path (res.body)
    Left _ -> pure unit
  pure unit
