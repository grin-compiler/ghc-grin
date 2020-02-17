module Main where

import Test.Framework (defaultMain)

import qualified Control.Monad.Catch.Tests

main :: IO ()
main = defaultMain
    [ Control.Monad.Catch.Tests.tests
    ]
