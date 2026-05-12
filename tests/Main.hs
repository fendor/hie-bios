module Main where

import BiosTests (getToolDeps, ignoreToolTests, verboseLogging, writeStackYamlFiles)
import qualified BiosTests
import qualified ParserTests

import Data.Foldable
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import Utils

main = do
  for_ [stderr, stdout] (`hSetBuffering` LineBuffering)
  writeStackYamlFiles
  toolDeps <- getToolDeps

  defaultMainWithIngredients (ignoreToolTests : verboseLogging : defaultIngredients) $
    testGroup
      "all"
      [ BiosTests.tests toolDeps
      , ParserTests.tests
      ]
