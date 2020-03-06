module Main where

import Control.Monad

import System.Environment

import Lambda.Syntax
import Lambda.ToStg
import StgLoopback

import Data.Binary

main :: IO ()
main = do
  fnames <- getArgs
  forM_ fnames $ \fname -> do
    putStrLn $ "reading " ++ fname
    program <- decodeFile fname :: IO Exp
    putStrLn "compiling"
    let (tyCons, binds) = toStg program
    compileProgram NCG tyCons binds
