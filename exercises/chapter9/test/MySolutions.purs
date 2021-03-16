module Test.MySolutions where

import Prelude

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, realpath, unlink, writeTextFile)

-- Note to reader: Add your solutions to this file
concatenateFiles firstPath secondPath outPath = do
    f1 <- readTextFile UTF8 firstPath
    f2 <- readTextFile UTF8 secondPath
    writeTextFile UTF8 outPath (f1 <> f2)