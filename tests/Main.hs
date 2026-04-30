module Main where

import qualified BiosTests
import qualified ParserTests

main = do
  for_ [stderr, stdout] (`hSetBuffering` LineBuffering)
  writeStackYamlFiles
  stackDep <- checkToolIsAvailable "stack"
  cabalDep <- checkToolIsAvailable "cabal"
  extraGhcDep <- checkToolIsAvailable extraGhc
