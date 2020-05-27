module StgLoopback where

-- Compiler
import GHC
import GHC.Paths ( libdir )
import GHC.Platform ( platformOS, osSubsectionsViaSymbols )
import GHC.Driver.CodeOutput
import GHC.Driver.Hooks
import GHC.Driver.Main
import GHC.Driver.Phases
import GHC.Driver.Pipeline
import GHC.Driver.Session
import GHC.Driver.Types
import GHC.Utils.Error
import GHC.Utils.Outputable

-- Stg Types
import GHC.Data.FastString
import GHC.Stg.FVs
import GHC.Stg.Lint
import GHC.Stg.Syntax
import GHC.Stg.Unarise
import GHC.Types.CostCentre
import GHC.Types.Module
import GHC.Types.Name.Set
import GHC.Data.Stream (Stream)
import qualified GHC.Data.Stream as Stream

-- Core Passes
import GHC.Cmm
import GHC.Cmm.Info (cmmToRawCmm )
import GHC.StgToCmm (codeGen)
import GHC.Types.Unique.Supply ( mkSplitUniqSupply, initUs_ )

import Control.Monad.Trans
import Control.Monad

import Data.Binary
import qualified Data.Set as Set

import qualified Stg.Syntax as C
import qualified Stg.Convert as C

import Data.List (isSuffixOf)
import System.FilePath

-------------------------------------------------------------------------------
-- Module
-------------------------------------------------------------------------------

modl :: Module
modl = mkModule mainUnitId (mkModuleName ":Main")

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

data Backend = NCG | LLVM


compileToObject :: Backend -> UnitId -> ModuleName -> ForeignStubs -> [TyCon] -> [StgTopBinding] -> FilePath -> IO ()
compileToObject backend unitId modName stubs tyCons topBinds_simple outputName = do
  runGhc (Just libdir) $ compileToObjectM backend unitId modName stubs tyCons topBinds_simple outputName

compileToObjectM :: Backend -> UnitId -> ModuleName -> ForeignStubs -> [TyCon] -> [StgTopBinding] -> FilePath -> Ghc ()
compileToObjectM backend unitId modName stubs tyCons topBinds_simple outputName = do
  dflags <- getSessionDynFlags

  let ccs       = emptyCollectedCCs :: CollectedCCs
      hpc       = emptyHpcInfo False

      this_mod  = mkModule unitId modName :: Module

      -- backend
      (target, link, outAsmFName) = case backend of
        LLVM  -> (HscLlvm, LlvmOpt, outputName -<.> ".ll")
        NCG   -> (HscAsm, As False, outputName -<.> ".s")

      beforeStgbinFName = outputName -<.> ".before_unarise.stgbin"
      afterStgbinFName  = outputName -<.> ".after_unarise.stgbin"

  liftIO $ do
    pure ()
{-
    putStrLn "==== Writing before unarise STG ===="
    encodeFile beforeStgbinFName $ C.cvtModule [] [] "whole program stg before unarise" mainUnitId (mkModuleName ":Main") topBinds_simple NoStubs []
-}
{-
    putStrLn "==== Lint STG ===="
    lintStgTopBindings dflags this_mod True "Manual" topBinds_simple
-}
  --us <- liftIO $ mkSplitUniqSupply 'u'
  --let topBinds = unarise us topBinds_simple
  let topBinds = topBinds_simple
{-
  liftIO $ do
    putStrLn "==== Writing after unarise STG ===="
    encodeFile afterStgbinFName $ C.cvtModule [] [] "whole program stg after unarise" mainUnitId (mkModuleName ":Main") topBinds NoStubs []
-}
  -- Compile
  dflags <- getSessionDynFlags
  pkgs <- setSessionDynFlags $
    dflags { hscTarget = target, ghcLink = NoLink }
    `gopt_set`  Opt_KeepSFiles
    `gopt_set`  Opt_KeepLlvmFiles
--    `dopt_set`  Opt_D_dump_cmm
--    `dopt_set`  Opt_D_dump_cmm_raw
--    `dopt_set`  Opt_D_dump_cmm_from_stg
{-
    `dopt_set`  Opt_D_dump_timings
    `gopt_set`  Opt_DoStgLinting
    `gopt_set`  Opt_DoCmmLinting
-}
    `gopt_set`  Opt_SplitSections -- HINT: linker based dead code elimination

  env <- getSession
  liftIO $ do
    newGen dflags env outAsmFName this_mod stubs tyCons ccs topBinds hpc
    oneShot env StopLn [(outAsmFName, Just link)]
  pure ()


compileProgram :: Backend -> [String] -> [String] -> [String] -> [String] -> ForeignStubs -> [TyCon] -> [StgTopBinding] -> IO ()
compileProgram backend incPaths libPaths ldOpts clikeFiles stubs tyCons topBinds_simple = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags

  liftIO $ do

    -- save exported external STG
    let stgFName = "whole_program_original.stgbin"
    putStrLn $ "writing " ++ stgFName
    encodeFile stgFName $ C.cvtModule [] [] "whole-program-stg" mainUnitId (mkModuleName ":Main") topBinds_simple NoStubs []

    putStrLn "==== STG ===="
--    putStrLn $ showSDoc dflags $ pprStgTopBindings topBinds_simple
    putStrLn "==== Lint STG ===="
    lintStgTopBindings dflags modl False "Manual" topBinds_simple

  us <- liftIO $ mkSplitUniqSupply 'g'
  let topBinds = unarise us topBinds_simple

  liftIO $ do
    let stgFName = "whole_program_unarised.stgbin"
    putStrLn $ "writing " ++ stgFName
    encodeFile stgFName $ C.cvtModule [] [] "whole-program-stg" mainUnitId (mkModuleName ":Main") topBinds NoStubs []

  -- construct STG program manually
  -- TODO: specify the following properly
{-
type CollectedCCs
  = ( [CostCentre]       -- local cost-centres that need to be decl'd
    , [CostCentreStack]  -- pre-defined "singleton" cost centre stacks
    )
-}
  let ccs       = emptyCollectedCCs :: CollectedCCs
      hpc       = emptyHpcInfo False

  -- backend
  let
    outFname  = "out.ll"

    (target, link) = case backend of
      LLVM  -> (HscLlvm, LlvmOpt)
      NCG   -> (HscAsm, As False)

  -- WORKAROUND: filter out rts includes
  let incPathsFixed = [p | p <- incPaths, not (isSuffixOf "rts-1.0/include" p)]

  -- Compile & Link
  dflags <- getSessionDynFlags
  pkgs <- setSessionDynFlags $
    dflags { hscTarget = target, ghcLink = LinkBinary, libraryPaths = libraryPaths dflags ++ libPaths, ldInputs = ldInputs dflags ++ map Option ldOpts
      , includePaths = addQuoteInclude (includePaths dflags) incPathsFixed
      }
    `gopt_set`  Opt_KeepSFiles
    `gopt_set`  Opt_KeepLlvmFiles
--    `dopt_set`  Opt_D_dump_cmm
--    `dopt_set`  Opt_D_dump_cmm_raw
--    `dopt_set`  Opt_D_dump_cmm_from_stg
    `dopt_set`  Opt_D_dump_timings
    `gopt_set`  Opt_DoStgLinting
    `gopt_set`  Opt_DoCmmLinting
    `gopt_set`  Opt_SplitSections -- HINT: linker based dead code elimination

  let libSet = Set.fromList ["rts"] -- "rts", "ghc-prim-cbits", "base-cbits", "integer-gmp-cbits"]
  dflags <- getSessionDynFlags
  let ignored_pkgs  = [IgnorePackage p |  p <- map (unpackFS . installedUnitIdFS) pkgs, Set.notMember p libSet]
      my_pkgs       = [ExposePackage p (PackageArg p)  (ModRenaming True []) | p <- Set.toList libSet]
  setSessionDynFlags $ dflags { ignorePackageFlags = ignored_pkgs, packageFlags = my_pkgs }
  dflags <- getSessionDynFlags

  env <- getSession
  liftIO $ do
    (_, stub_c_exists, _, _) <- newGen dflags env outFname modl stubs tyCons ccs topBinds hpc
    let stubC = case stub_c_exists of
          Nothing -> []
          Just f  -> [(f, Nothing)]
    oneShot env StopLn $ (outFname, Just link) : [(c, Nothing) | c <- clikeFiles] ++ stubC
  pure ()

-------------
-- from GHC
-------------

newGen :: DynFlags
       -> HscEnv
       -> FilePath
       -> Module
       -> ForeignStubs
       -> [TyCon]
       -> CollectedCCs
       -> [StgTopBinding]
       -> HpcInfo
       -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], NameSet)
newGen dflags hsc_env output_filename this_mod foreign_stubs data_tycons cost_centre_info stg_binds hpc_info = do
  -- TODO: add these to parameters
  let location = ModLocation
        { ml_hs_file  = Just $ output_filename -<.> ".hs"
        , ml_hi_file  = output_filename -<.> ".hi"
        , ml_obj_file = output_filename -<.> ".o"
        , ml_hie_file = output_filename -<.> ".hie"
        }
      foreign_files = []
      dependencies = [] -- only used for HscC target

  cmms <- {-# SCC "StgToCmm" #-}
                  doCodeGen hsc_env this_mod data_tycons
                      cost_centre_info
                      stg_binds hpc_info

  ------------------  Code output -----------------------
  rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
            lookupHook cmmToRawCmmHook
              (\dflg _ -> cmmToRawCmm dflg) dflags dflags (Just this_mod) cmms

  let dump a = do
        unless (null a) $
          dumpIfSet_dyn dflags Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (ppr a)
        return a
      rawcmms1 = Stream.mapM dump rawcmms0

  (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps, caf_infos)
      <- {-# SCC "codeOutput" #-}
        codeOutput dflags this_mod output_filename location
        foreign_stubs foreign_files dependencies rawcmms1
  return (output_filename, stub_c_exists, foreign_fps, caf_infos)
