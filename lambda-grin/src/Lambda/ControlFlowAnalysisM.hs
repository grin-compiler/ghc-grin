{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.ControlFlowAnalysisM where

-- NOTE: only when the whole program is available

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map

import System.FilePath
import System.Process
import System.IO
import System.IO.Temp

import Lambda.Syntax2
import Lambda.ToDatalog

controlFlowAnalysisM :: [String] -> Program -> IO (Map String [[Text]])
controlFlowAnalysisM initialReachable prg = do

  putStrLn "controlFlowAnalysisM:"
  putStrLn "export facts"
  tmpSys <- getCanonicalTemporaryDirectory
  tmpCfa <- createTempDirectory tmpSys "lambda-cfa"

  programToFactsM tmpCfa prg

  let srcFile = tmpCfa </> "InitialReachable.facts"
  putStrLn srcFile
  writeFile srcFile $ unlines initialReachable

  putStrLn "run: lambda-cfa"
  callProcess "lambda-cfa" ["--output=" ++ tmpCfa, "--facts=" ++ tmpCfa, "--jobs=4"]

  putStrLn "read back result"
  let result =
        -- CFA
        [ "ApplyChain", "PNode", "PNodeArgument", "Called"
        -- MISC
        , "CallPNode1", "CallPNode2", "PointsTo", "TypeVarPointsTo"
        -- LVA
        , "ReachableCode", "Reachable", "DeadCode", "DeadExternal", "ReachableExternal"
        -- CBy
        , "ExternalOrigin", "NodeOrigin", "TagValue"
        -- AST
        , "HasInst", "RetTup1Node0", "RetTup"
        -- Check
        , "Error", "MissingValue"
        -- PrimOp
        , "Array"
        , "MVar", "MVarDef"
        , "RaisedEx"
        , "TVar"
        , "Spark"
        , "WeakPtr" , "WeakFinalizer"
        , "MutVar"
        , "StablePtr"
        ]
  Map.fromList <$> forM result (\name -> do
    row <- map Text.words . Text.lines <$> Text.readFile (tmpCfa </> name ++ ".csv")
    pure (name, row)
    )
