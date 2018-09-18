module Main (main) where

import qualified System.FilePath as FilePath

import qualified Test.Framework as Framework
import qualified Test.Framework.Providers.HUnit as HUnit

import qualified Test

main :: IO ()
main = Framework.defaultMain tests

tests :: [Framework.Test]
tests = [goldensTests]

-- | All the tests that depend on reading an input file with a simple program,
-- parsing and optimizing it and then comparing with an expected output.
goldensTests :: Framework.Test
goldensTests = Framework.testGroup "Goldens tests"
    [ HUnit.testCase inputFile $ compareWithExpected inputFile expectedFile
    | (inputFile, expectedFile) <- zip inputFiles expectedFiles ]
  where
    compareWithExpected = Test.optTest
    inputFiles = [ basePath FilePath.</> test | test <- testFileNames ]
    expectedFiles = [ basePath FilePath.</> test FilePath.<.> "expected"
                    | test <- testFileNames ]
    basePath = "testing" FilePath.</> "tests"
    testFileNames =
        [ "test1"
        , "test2"
        , "test3"
        , "test4"
        , "test5"
        , "test6"
        , "test7"
        , "if-test"
        , "if-test2"
        , "if-test3"
        , "if-test4"
        ]
