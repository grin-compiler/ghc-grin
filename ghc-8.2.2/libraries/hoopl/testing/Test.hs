{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Test (parseTest, evalTest, optTest) where

import Compiler.Hoopl
import Control.Monad.Except
import System.Exit

import qualified Ast as A
import qualified Ir2ast as Ia
import Ast2ir
import ConstProp
import Eval  (evalProg, ErrorM)
import IR
import Live
import Parse (parseCode)
import Simplify
parse :: String -> String -> ErrorM (M [(IdLabelMap, Proc)])
parse file text =
  case parseCode file text of
    Left  err -> throwError $ show err
    Right ps  -> return $ mapM astToIR ps

parseTest :: String -> IO ()
parseTest file =
  do text <- readFile file
     case parse file text of
       Left err -> error err
       Right p  -> mapM (putStrLn . showProc . snd) (runSimpleUniqueMonad $ runWithFuel 0 p) >> return ()

evalTest' :: String -> String -> ErrorM String
evalTest' file text =
  do procs   <- parse file text
     (_, vs) <- (testProg . snd . unzip) (runSimpleUniqueMonad $ runWithFuel 0 procs)
     return $ "returning: " ++ show vs
  where
    testProg procs@(Proc {name, args} : _) = evalProg procs vsupply name (toV args)
    testProg _ = throwError "No procedures in test program"
    toV args = [I n | (n, _) <- zip [3..] args]
    vsupply = [I x | x <- [5..]]

evalTest :: String -> IO ()
evalTest file =
  do text    <- readFile file
     case evalTest' file text of
       Left err -> error err
       Right  s -> putStrLn s

optTest' :: M [Proc] -> ErrorM (M [Proc])
optTest' procs =
  return $ procs >>= mapM optProc
  where
    optProc proc@(Proc {entry, body, args}) =
      do { (body',  _, _) <- analyzeAndRewriteFwd fwd (JustC [entry]) body
                             (mapSingleton entry (initFact args))
         ; (body'', _, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) body' mapEmpty
         ; return $ proc { body = body'' } }
    -- With debugging info:
    -- fwd  = debugFwdJoins trace (const True) $ FwdPass { fp_lattice = constLattice, fp_transfer = varHasLit
    --                                      , fp_rewrite = constProp `thenFwdRw` simplify }
    fwd  = constPropPass
    bwd  = BwdPass { bp_lattice = liveLattice, bp_transfer = liveness
                   , bp_rewrite = deadAsstElim }

constPropPass :: FuelMonad m => FwdPass m Insn ConstFact
-- @ start cprop.tex

----------------------------------------
-- Defining the forward dataflow pass
constPropPass = FwdPass
  { fp_lattice  = constLattice
  , fp_transfer = varHasLit
  , fp_rewrite  = constProp `thenFwdRw` simplify }
-- @ end cprop.tex

toAst :: [(IdLabelMap, Proc)] -> [A.Proc]
toAst l = fmap (uncurry Ia.irToAst) l

compareAst :: [A.Proc] -> [A.Proc] -> IO ()
compareAst [] [] = return ()
compareAst (r:results) (e:expected) =
  if r == e
  then compareAst results expected
  else
    do { putStrLn "expecting"
       ; putStrLn $ A.showProc e
       ; putStrLn "resulting"
       ; putStrLn $ A.showProc r
       ; putStrLn "the result does not match the expected, abort the test!!!!"
       ; exitFailure
       }
compareAst results expected = do { putStrLn "expecting"
                                 ; mapM_ (putStrLn . A.showProc) expected
                                 ; putStrLn "resulting"
                                 ; mapM_ (putStrLn . A.showProc) results
                                 ; putStrLn "the result does not match the expected, abort the test!!!!"
                                 ; exitFailure
                                 }



optTest :: String -> String -> IO ()
optTest file expectedFile =
  do text    <- readFile file
     expectedText <- readFile expectedFile
     case (parse file text, parse expectedFile expectedText) of
       (Left err, _) -> error err
       (_, Left err) -> error err
       (Right lps, Right exps) ->
         case optTest' (liftM (snd . unzip) lps) of
           Left err -> error err
           Right p  -> do { let opted = runSimpleUniqueMonad $ runWithFuel fuel p
                                lbmaps = runSimpleUniqueMonad $ runWithFuel fuel (liftM (fst . unzip) lps)
                                expected = runSimpleUniqueMonad $ runWithFuel fuel exps
                          ; compareAst (toAst (zip lbmaps opted)) (toAst expected)
                          }
  where
    fuel = 9999



{-- Properties to test:

  1. Is the fixpoint complete (maps all blocks to facts)?
  2. Is the computed fixpoint actually a fixpoint?
  3. Random test generation.

--}
