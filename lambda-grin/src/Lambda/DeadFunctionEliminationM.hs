{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.DeadFunctionEliminationM where

-- NOTE: only when the whole program is available

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.FilePath
import System.Process
import System.IO
import System.IO.Temp

import Lambda.Syntax2
import Transformations.Util

deadFunctionEliminationM :: [String] -> Program -> IO Program
deadFunctionEliminationM liveSources prg@(Program exts cons sdata defs) = do

  putStrLn "export facts"
  tmpSys <- getCanonicalTemporaryDirectory
  tmpDfe <- createTempDirectory tmpSys "lambda-dfe"
  let factFile = tmpDfe </> "DefReference.facts"
  h <- openFile factFile WriteMode
  putStrLn factFile
  collectNamesM h prg
  hFlush h
  hClose h

  let srcFile = tmpDfe </> "LiveSource.facts"
  putStrLn srcFile
  writeFile srcFile $ unlines liveSources

  putStrLn "run: deadFunctionEliminationM"
  callProcess "live_def_analysis" ["--output=" ++ tmpDfe, "--facts=" ++ tmpDfe, "--jobs=4"]

  putStrLn "read back result"
  liveSet <- Set.fromList . map (packName . Text.unpack) . Text.lines <$> Text.readFile (tmpDfe </> "LiveName.csv")

  let liveExts  = [e | e <- exts, Set.member (eName e) liveSet]
      liveDefs  = [d | d@(Def name _ _) <- defs, Set.member name liveSet]
      liveSData = [d | d <- sdata, Set.member (sName d) liveSet]
      liveCons  = cons -- TODO

  pure (Program liveExts liveCons liveSData liveDefs)

type SM = StateT (Set Name) IO

collectNamesM :: Handle -> Program -> IO ()
collectNamesM h (Program exts cons sdata defs) = do
  let defSet :: Set Name
      defSet = Set.fromList [name | (Def name _ _) <- defs]

      extSet :: Set Name
      extSet = Set.fromList [eName e | e <- exts]

      sdataSet :: Set Name
      sdataSet = Set.fromList [sName s | s <- sdata]

      nameSet :: Set Name
      nameSet = mconcat [defSet, extSet, sdataSet]

      folder :: String -> ExpF () -> SM ()
      folder defName = \case
        AppF name args -> mapM_ (add defName) $ name : args
        ConF _ args    -> mapM_ (add defName) args
        VarF name      -> add defName name
        CaseF name _   -> add defName name
        exp -> pure ()

      add :: String -> Name -> SM ()
      add defName n = when (Set.member n nameSet) $ do
        s <- get
        case Set.member n s of
          True  -> pure ()
          False -> do
            liftIO $ do
              --putStrLn $ "add: " ++ unpackName n
              hPutStrLn h $ defName <> "\t" <> unpackName n
            put $ Set.insert n s

  forM_ defs $ \def@(Def name _ _) -> do
    execStateT (cataM (folder (unpackName name)) def) mempty
    pure ()
