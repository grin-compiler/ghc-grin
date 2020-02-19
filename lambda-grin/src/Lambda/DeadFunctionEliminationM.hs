{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings, RecordWildCards #-}
module Lambda.DeadFunctionEliminationM (deadFunctionEliminationM) where

-- NOTE: only when the whole program is available

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.FilePath
import System.Process
import System.IO
import System.IO.Temp

import Lambda.Syntax
import Transformations.Util

deadFunctionEliminationM :: [String] -> Program -> IO Program
deadFunctionEliminationM liveSources prg@(Program exts cons sdata defs) = do

  putStrLn "export facts"
  tmpSys <- getCanonicalTemporaryDirectory
  tmpDfe <- createTempDirectory tmpSys "lambda-dfe"
  let openFact n = do
        let factFile = tmpDfe </> n
        h <- openFile factFile WriteMode
        putStrLn factFile
        pure h

      closeFact h = do
        hFlush h
        hClose h

  -- write input facts
  factDefRef <- openFact "DefReference.facts"
  factConRef <- openFact "ConReference.facts"
  collectNamesM factDefRef factConRef prg
  closeFact factDefRef
  closeFact factConRef

  factConGroup <- openFact "ConGroup.facts"
  mapM_ (collectConsM factConGroup) cons
  closeFact factConGroup

  let srcFile = tmpDfe </> "LiveSource.facts"
  putStrLn srcFile
  writeFile srcFile $ unlines liveSources

  -- run analysis
  putStrLn "run: deadFunctionEliminationM"
  callProcess "live_def_analysis" ["--output=" ++ tmpDfe, "--facts=" ++ tmpDfe, "--jobs=4"]

  -- read output fatcs
  putStrLn "read back result"
  liveDefSet <- Set.fromList . map (packName . Text.unpack) . Text.lines <$> Text.readFile (tmpDfe </> "LiveDefName.csv")
  liveConGroupSet <- Set.fromList . map (packName . Text.unpack) . Text.lines <$> Text.readFile (tmpDfe </> "LiveGroupName.csv")

  let liveExts  = [e | e <- exts, Set.member (eName e) liveDefSet]
      liveDefs  = [d | d@(Def name _ _) <- defs, Set.member name liveDefSet]
      liveSData = [d | d <- sdata, Set.member (sName d) liveDefSet]
      liveCons  = [c | c <- cons, Set.member (cgName c) liveConGroupSet]

  pure (Program liveExts liveCons liveSData liveDefs)

collectConsM :: Handle -> ConGroup -> IO ()
collectConsM h ConGroup{..} = do
  forM_ cgCons $ \ConSpec{..} -> do
    hPutStrLn h $ unpackName cgName <> "\t" <> unpackName csName

collectNamesM :: Handle -> Handle -> Program -> IO ()
collectNamesM hDef hCon (Program exts _cons sdata defs) = do
  let defSet :: Set Name
      defSet = Set.fromList [name | (Def name _ _) <- defs]

      extSet :: Set Name
      extSet = Set.fromList [eName e | e <- exts]

      sdataSet :: Set Name
      sdataSet = Set.fromList [sName s | s <- sdata]

      nameSet :: Set Name
      nameSet = mconcat [defSet, extSet, sdataSet]

      folder :: String -> ExpF () -> IO ()
      folder defName = \case
        AppF name args -> mapM_ (add hDef defName) $ name : args
        ConF con args  -> mapM_ (add hDef defName) args >> add hCon defName con
        VarF name      -> add hDef defName name
        CaseF name _   -> add hDef defName name
        AltF _ (NodePat con _) _ ->  add hCon defName con
        exp -> pure ()

      add :: Handle -> String -> Name -> IO ()
      add h defName n = do
            when (Set.member n nameSet) $ do
              hPutStrLn h $ defName <> "\t" <> unpackName n

  forM_ defs $ \def@(Def name _ _) -> do
    cataM (folder (unpackName name)) def
