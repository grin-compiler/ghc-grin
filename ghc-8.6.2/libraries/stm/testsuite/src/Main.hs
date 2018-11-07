{-# LANGUAGE CPP #-}

module Main where

import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit

import qualified Issue9
import qualified Issue17
import qualified Stm052
import qualified Stm064
import qualified Stm065

main :: IO ()
main = do
    putStrLn ("'stm' version under test: " ++ VERSION_stm)
    defaultMain tests
  where
    tests = [
      testGroup "regression"
        [ testCase "issue #9" Issue9.main
        , testCase "issue #17" Issue17.main
        , testCase "stm052" Stm052.main
        , testCase "stm064" Stm064.main
        , testCase "stm065" Stm065.main
        ]
      ]

