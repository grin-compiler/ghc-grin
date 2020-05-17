module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List (isPrefixOf, groupBy, sortBy, foldl')
import Data.List.Split
import Control.Monad
import Text.Printf
import Control.Concurrent.Async.Pool

import System.Environment
import System.FilePath
import System.FilePath.Find
import System.Process

import qualified Lambda.GHCSymbols as GHCSymbols
import Lambda.Name
--import Lambda.Syntax
--import Lambda.ToStg
import StgLoopback
import Stg.Util
import Stg.Syntax

import Stg.WriteDfeFacts

import Data.Binary

import qualified Data.ByteString.Char8 as BS8

--import GHC.Stg.Syntax (StgTopBinding)
import qualified GHC.Driver.Types as GHC
import qualified Outputable as GHC

moduleStgbin :: String -> FilePath
moduleStgbin moduleName = replaceEq '.' '/' moduleName <.> "o_stgbin"

replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from to = map (\cur -> if cur == from then to else cur)

parseSection :: [String] -> String -> [String]
parseSection content n = map (read . tail) . takeWhile (isPrefixOf "-") . tail . dropWhile (not . isPrefixOf n) $ content

getAppLibs :: FilePath -> IO ([String], [String], [String], [String])
getAppLibs fname = do
  content <- lines <$> readFile fname
  let paths     = parseSection content "pkg_lib_paths:"
      incPaths  = parseSection content "pkg_include_paths:"
      [root]    = parseSection content "root:"
      cObjFiles = parseSection content "extra_ld_inputs:"
      ldFlags   = parseSection content "other_flags:"
      extraLibs = parseSection content "extra_libs:"
      pkgLibs   = parseSection content "package_hs_libs:"
      cLibs     = filter (isPrefixOf "-lC") pkgLibs
  archives <- Set.fromList . map takeFileName . concat <$> mapM (find always (extension ==? ".a")) paths
  let cbits   = [o ++ ".cbits" | o <- pkgLibs, Set.member ("lib" ++ drop 2 o ++ ".cbits.a") archives]
      stubs   = [o ++ ".stubs" | o <- pkgLibs, Set.member ("lib" ++ drop 2 o ++ ".stubs.a") archives]
      ldOpts  = concat [stubs, cbits, cLibs, ldFlags, extraLibs]
  pure (incPaths, paths, ldOpts, [root </> o | o <- cObjFiles])

getPkgStgbins :: FilePath -> IO [FilePath]
getPkgStgbins stglibPath = do
  fname <- head <$> find always (extension ==? ".stglib") stglibPath
  content <- lines <$> readFile fname
  let modules = parseSection content "modules:"
  pure [stglibPath </> moduleStgbin m | m <- modules]

getStgbins :: FilePath -> IO [FilePath]
getStgbins fname = do
  content <- lines <$> readFile fname
  let paths   = parseSection content "pkg_lib_paths:"
      [root]  = parseSection content "root:"
      o_files = parseSection content "o_files:"
  stgbins <- mapM getPkgStgbins paths
  pure $ [root </> (o ++ "_stgbin") | o <- o_files ] ++ concat stgbins

{-
  TODO: LTO-DFE
    done - app module pruning
    - ext stg name collector pass
    - cli tool to generate facts

    - collect facts + run LTO-DFE + export results
    - call gen-obj + import liveness result + prune top level bindings

-}

collectProgramModules :: [FilePath] -> String -> String -> IO [FilePath]
collectProgramModules stgbinFileNames unitId mod = do
  -- filter dependenies only
  (fexportedList, depList) <- fmap unzip . forM stgbinFileNames $ \fname -> do
    (_, u, m, _, hasForeignExport, deps) <- readStgbinInfo fname
    let fexport = if hasForeignExport then Just (u, m) else Nothing
    pure (fexport, ((u, m), [(du, dm) | (du, dl) <- deps, dm <- dl]))
  let fnameMap  = Map.fromList $ zip (map fst depList) stgbinFileNames
      mnameMap  = Map.fromList $ zip stgbinFileNames (map fst depList)
      depMap    = Map.fromList depList
      calcDep s n
        | Set.member n s = s
        | Just l <- Map.lookup n depMap = foldl' calcDep (Set.insert n s) l
        | otherwise = Set.insert n s -- error $ printf "missing module: %s" . show $ getModuleName n

      keyMain = (UnitId $ BS8.pack unitId, ModuleName $ BS8.pack mod)
      prunedDeps = catMaybes [Map.lookup m fnameMap | m <- Set.toList $ foldl calcDep mempty $ keyMain : rtsDeps ++ catMaybes fexportedList]
      rtsDeps = [(UnitId $ BS8.pack u, ModuleName $ BS8.pack m) | (u, m, _) <- catMaybes $ map (decodePackageQualifiedName . packName) GHCSymbols.liveSymbols]

  putStrLn $ "all modules: " ++ show (length stgbinFileNames)
  putStrLn $ "app modules: " ++ show (length prunedDeps)
  putStrLn $ "app dependencies:\n"
  forM_ [mnameMap Map.! fname | fname <- prunedDeps] $ \(UnitId uid, ModuleName mod) -> do
    printf "%-60s %s\n" (BS8.unpack uid) (BS8.unpack mod)
  pure prunedDeps

genProgramDfeFacts :: [FilePath] -> IO ()
genProgramDfeFacts stgbinFileNames = do
  putStrLn "generate datalog facts for whole stg program dead function elimination"
  forM_ stgbinFileNames $ \stgbinName -> do
    extStgModule <- readStgbin stgbinName
    writeDfeFacts stgbinName extStgModule

main :: IO ()
main = do
  [stgAppFname] <- getArgs

  putStrLn "compile STG modules"
  stgBins <- getStgbins stgAppFname

  appStgBins <- collectProgramModules stgBins "main" "Main"

  -- DFE pass
  genProgramDfeFacts appStgBins

  withTaskGroup 4 $ \g -> do
    mapTasks g [callProcess "gen-obj" f | f <- chunksOf 1 appStgBins]

  let oStg = [s ++ ".o" | s <- appStgBins]
  o@(incPaths, libPaths, ldOpts, clikeFiles) <- getAppLibs stgAppFname

  let cg = NCG

  putStrLn $ "linking exe"
  compileProgram cg incPaths libPaths ldOpts (clikeFiles ++ oStg) GHC.NoStubs [] []
