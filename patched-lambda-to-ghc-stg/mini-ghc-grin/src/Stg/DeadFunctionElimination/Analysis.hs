{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, OverloadedStrings #-}
module Stg.DeadFunctionElimination.Analysis where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import System.Directory
import System.FilePath
import System.Process
import System.IO
import System.IO.Temp

import qualified Lambda.GHCSymbols as GHCSymbols

{-
  TODO:
    done - collect module facts
    done - run analysis
    done - write module liveness
-}

livenessAnalysisM :: [FilePath] -> IO ()
livenessAnalysisM = livenessAnalysisImplM False

livenessAnalysisLogM :: [FilePath] -> IO ()
livenessAnalysisLogM = livenessAnalysisImplM True

livenessAnalysisImplM :: Bool -> [FilePath] -> IO ()
livenessAnalysisImplM log stgBins = do

  tmpSys <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory tmpSys "ext-stg-liveness"

  when log $ do
    putStrLn "livenessAnalysisM:"
    putStrLn $ "export facts to:"
    putStrLn tmpDir

  -- prepare input facts

  let fixes =
        [ "ghc-prim_GHC.Types.True"
        ]

  mergeInputFacts stgBins tmpDir
  -- add GHC exported symbols to LiveSources
  -- WORKAROUND: add "main_Main.main" keep "main_:Main.main" alive because it lives in the Main module
  appendFile (tmpDir </> "LiveSource.facts") $ unlines $ "main_Main.main" : GHCSymbols.liveSymbols ++ fixes

  -- run liveness analysis

  when log $ putStrLn "run: ext-stg-liveness"
  callProcess "ext-stg-liveness" ["--output=" ++ tmpDir, "--facts=" ++ tmpDir, "--jobs=4"]

  when log $ putStrLn "read back result"
  copyFile (tmpDir </> "LiveFunName.csv") "LiveFunName.csv"
  copyFile (tmpDir </> "LiveDataConName.csv") "LiveDataConName.csv"

dfeInputFacts :: [String]
dfeInputFacts =
  [ "LiveSource.facts"
  , "TyCon.facts"
  , "TyConReference.facts"
  , "DataConReference.facts"
  , "FunReference.facts"
  ]

mergeInputFacts :: [FilePath] -> FilePath -> IO ()
mergeInputFacts stgBins tmpDir = do
  forM_ dfeInputFacts $ \factName -> do
    let factFile = tmpDir </> factName
    forM_ stgBins $ \stgFname -> do
      BS.readFile (stgFname -<.> factName) >>= BS.appendFile factFile

{-
// input fatcs
.decl TyCon(tycon:Name, datacon:Name)
.input TyCon

.decl TyConReference(fun:Name, tycon:Name)
.input TyConReference

.decl DataConReference(fun:Name, datacon:Name)
.input DataConReference

.decl FunReference(fun:Name, funref:Name)
.input FunReference

.decl LiveSource(fun:Name)
.input LiveSource

// output fatcs
.decl LiveFunName(fun:Name)
.output LiveFunName

.decl LiveTyConName(tycon:Name)
.output LiveTyConName

.decl LiveDataConName(datacon:Name)
.output LiveDataConName
-}