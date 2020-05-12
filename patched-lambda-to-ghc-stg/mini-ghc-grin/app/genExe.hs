module Main where

import qualified Data.Set as Set
import Data.List (isPrefixOf, groupBy, sortBy)
import Data.List.Split
import Control.Monad
import Text.Printf

import System.Environment
import System.FilePath
import System.FilePath.Find
import System.Process

--import Lambda.Name
--import Lambda.Syntax
--import Lambda.ToStg
import StgLoopback
import Stg.Util
import Stg.Syntax (ForeignStubs(..))

import Data.Binary

import qualified Data.ByteString.Char8 as BS8

import GHC.Stg.Syntax (StgTopBinding)
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
readStubs :: String -> IO GHC.ForeignStubs
readStubs fname = do
  (hStub, cStub) <- decodeFile (fname ++ ".stubsbin") :: IO (BS8.ByteString, BS8.ByteString)
  let hStr = BS8.unpack hStub
      cStr = BS8.unpack cStub
  pure $ if null hStr && null cStr
    then GHC.NoStubs
    else GHC.ForeignStubs (GHC.text hStr) (GHC.text cStr)


main :: IO ()
main = do
  [fname] <- getArgs
  let lambdaName = fname ++ ".lambdabin"
  putStrLn $ "reading " ++ lambdaName
  program <- decodeFile lambdaName :: IO Program
  stubs <- readStubs fname
  putStrLn "compiling"
  let (tyCons, binds) = toStgWithNames program
  o@(incPaths, libPaths, ldOpts, clikeFiles) <- getAppLibs fname
  print o

  --let stgChunks = [(printf "%s.%04d.o" fname i, s) | (i, s) <- zip [0 :: Int ..] (chunksOf 30 binds)]
  let stgChunks   = getStgChunks fname binds
  forM_ stgChunks $ \(oName, stgBinds) -> do
    printf "%5d\t%s\n" (length stgBinds) oName

  let cg = NCG

  chunkFiles <- forM stgChunks $ \(oName, stgBinds) -> do
    putStrLn "compiling:"
    printf "%5d\t%s\n" (length stgBinds) oName
    compileToObject cg [] stgBinds oName
    pure oName

  putStrLn $ "compiling and linking"
  compileProgram cg incPaths libPaths ldOpts (clikeFiles ++ chunkFiles) stubs tyCons []

getStgChunks :: FilePath -> [(Name, StgTopBinding)] -> [(FilePath, [StgTopBinding])]
getStgChunks appName l = zipWith mkChunk [0 :: Int ..] $ groupBy (\a b -> fst a == fst b) [(mkOutName n, s) | (n, s) <- l]
  where
    mkChunk i xs@((n,_):_) = (printf "%s.%s.%02d.o" appName n i, map snd xs)
    mkOutName n = case decodePackageQualifiedName n of
      Nothing             -> error $ "illegal name: " ++ unpackName n
      Just (uid, mod, _)  -> uid ++ "_" ++ mod

sortChunks :: [(FilePath, [StgTopBinding])] -> [(FilePath, [StgTopBinding])]
sortChunks l = map snd $ sortBy (\a b -> compare (fst b) (fst a)) [(length $ snd x, x) | x <- l]
-}

main :: IO ()
main = do
  [stgAppFname] <- getArgs

  putStrLn "compile STG modules"
  stgBins <- getStgbins stgAppFname

  --callProcess "gen-obj" stgBins
  forM_ stgBins $ \f -> callProcess "gen-obj" [f]

  let oStg = [s ++ ".o" | s <- stgBins]
  o@(incPaths, libPaths, ldOpts, clikeFiles) <- getAppLibs stgAppFname

  let cg = NCG

  putStrLn $ "linking exe"
  compileProgram cg incPaths libPaths ldOpts (clikeFiles ++ oStg) GHC.NoStubs [] []
