{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Text.Printf
import Control.Monad

import System.FilePath
import System.Environment
import System.Exit

import GhcDump_StgAst (ModuleName(..))
import GhcDump.StgUtil

import Lambda.FromStg
import Lambda.Syntax
import Lambda.Pretty
import Lambda.CodeGen
import Lambda.Lint
import Lambda.StaticSingleAssignment
import Lambda.ClosureConversion
import Lambda.DeadFunctionElimination
import Pipeline.Pipeline

import Data.Maybe
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Binary

import Text.PrettyPrint.ANSI.Leijen (ondullblack, putDoc, plain, pretty)

import System.Posix.Resource

data Opts
  = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  }

showUsage = do putStrLn "Usage: ghc-grin <stg-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.out") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

deriving instance Show ResourceLimit
deriving instance Show ResourceLimits

cg_main :: Opts -> IO ()
cg_main opts = do
  let inputLen = length $ inputs opts
  printf "input length: %d\n" inputLen
  r <- getResourceLimit ResourceOpenFiles
  print r
  let ResourceLimits (ResourceLimit minNum) (ResourceLimit maxNum) = r
  setResourceLimit ResourceOpenFiles $ ResourceLimits (ResourceLimit $ max minNum $ min (fromIntegral inputLen + minNum) maxNum) (ResourceLimit maxNum)
  -- filter dependenies only
  depList <- mapM readDumpInfo (inputs opts)
  let fnameMap  = Map.fromList $ zip (map fst depList) (inputs opts)
      depMap    = Map.fromList depList
      calcDep s n
        | Set.member n s = s
        | Just l <- Map.lookup n depMap = foldl' calcDep (Set.insert n s) l
        | otherwise = Set.insert n s -- error $ printf "missing module: %s" . show $ getModuleName n

      modMain = ModuleName $ BS8.pack "Main"
      prunedDeps = catMaybes [Map.lookup m fnameMap | m <- Set.toList $ calcDep mempty modMain]

  -- compile pruned program
  defList <- forM prunedDeps $ \fname -> do
    putStrLn $ "loading " ++ fname
    stgModule <- readDump fname
    program@(Program defs) <- codegenLambda stgModule

    let lambdaName = replaceExtension fname "lambda"
    putStrLn lambdaName
    writeFile lambdaName . show . plain $ pretty program

    pure defs

    {-
    let lambdaGrin = codegenGrin program
    void $ pipeline pipelineOpts lambdaGrin
      [ SaveGrin "from-lambda.grin"
      , T GenerateEval
      , SaveGrin (output opts)
      , PrintGrin ondullblack
      ]
    -}
  let sortDefs (Program defs) = Program . Map.elems $ Map.fromList [(n,d) | d@(Def n _ _) <- defs]
      wholeProgramBloat = eliminateLams [] $ singleStaticAssignment $ Program $ concat defList
      wholeProgram      = sortDefs $ deadFunctionElimination wholeProgramBloat
      output_fn         = output opts
  writeFile (output_fn ++ ".lambda") . show . plain $ pretty wholeProgram
  lintLambda wholeProgram
  printf "all: %d pruned: %d\n" (length $ inputs opts) (length prunedDeps)
  let Program defsStripped  = wholeProgram
      Program defsBloat     = wholeProgramBloat
  printf "bloat    lambda def count: %d\n" (length defsBloat)
  printf "stripped lambda def count: %d\n" (length defsStripped)

  let pset = Set.fromList prunedDeps
      aset = Set.fromList $ inputs opts
  printf "dead modules:\n%s" (unlines $ Set.toList $ aset Set.\\ pset)

  BSL.writeFile (output_fn ++ ".lambdabin") $ encode wholeProgram

  let lambdaGrin = codegenGrin wholeProgram
  writeFile (output_fn ++ ".grin") $ show $ plain $ pretty lambdaGrin

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else cg_main opts

pipelineOpts :: PipelineOpts
pipelineOpts = defaultOpts
  { _poOutputDir = ".ghc-grin"
  , _poFailOnLint = True
  }
