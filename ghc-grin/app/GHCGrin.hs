module Main where

import Control.Monad

import System.FilePath
import System.Environment
import System.Exit

import GhcDump.StgUtil

import Lambda.FromDumpStg
import Lambda.Syntax
import Lambda.Pretty
import Lambda.CodeGen
import Lambda.Lint
import Pipeline.Pipeline

import Text.PrettyPrint.ANSI.Leijen (ondullblack, putDoc, plain, pretty)

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

cg_main :: Opts -> IO ()
cg_main opts = do
  defList <- forM (inputs opts) $ \fname -> do
    putStrLn $ "loading " ++ fname
    stgModule <- readDump fname
    putStrLn $ "loaded " ++ fname
    program@(Program defs) <- codegenLambda stgModule

    let lambdaName = replaceExtension fname "lambda"
    --putStrLn lambdaName
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
  let wholeProgram = Program $ concat defList
  --writeFile "whole_program.lambda" . show . plain $ pretty wholeProgram
  lintLambda wholeProgram

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
