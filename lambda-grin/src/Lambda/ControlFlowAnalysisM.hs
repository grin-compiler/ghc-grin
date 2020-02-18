{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.ControlFlowAnalysisM where

-- NOTE: only when the whole program is available

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.FilePath
import System.Process
import System.IO
import System.IO.Temp

import Lambda.Syntax
import Lambda.ToDatalog

controlFlowAnalysisM :: [String] -> Program -> IO (Map String [[Text]])
controlFlowAnalysisM = controlFlowAnalysisImplM False

controlFlowAnalysisLogM :: [String] -> Program -> IO (Map String [[Text]])
controlFlowAnalysisLogM = controlFlowAnalysisImplM True

controlFlowAnalysisImplM :: Bool -> [String] -> Program -> IO (Map String [[Text]])
controlFlowAnalysisImplM log initialReachable prg = do

  tmpSys <- getCanonicalTemporaryDirectory
  tmpCfa <- createTempDirectory tmpSys "lambda-cfa"

  when log $ do
    putStrLn "controlFlowAnalysisM:"
    putStrLn $ "export facts to:"
    putStrLn tmpCfa

  programToFactsM log tmpCfa prg

  let srcFile = tmpCfa </> "InitialReachable.facts"
  when log $ putStrLn srcFile
  writeFile srcFile $ unlines initialReachable

  when log $ putStrLn "run: lambda-cfa"
  callProcess "lambda-cfa" ["--output=" ++ tmpCfa, "--facts=" ++ tmpCfa, "--jobs=4"]

  when log $ putStrLn "read back result"
  result <- filter (\n -> takeExtension n == ".csv") <$> listDirectory tmpCfa
  Map.fromList <$> forM result (\fname -> do
    row <- map (Text.splitOn "\t") . Text.lines <$> Text.readFile (tmpCfa </> fname)
    pure (takeBaseName fname, row)
    )
