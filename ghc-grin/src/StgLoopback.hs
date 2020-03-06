module StgLoopback where

-- Compiler
import GHC
import ErrUtils
import GHC.Platform ( platformOS, osSubsectionsViaSymbols )
import Outputable
import GHC.Paths ( libdir )
--import HscTypes
--import DriverPhases
--import DynFlags
--import DriverPipeline
--import HscMain
--import CodeOutput
--import Hooks
import GHC.Driver.Session
import GHC.Driver.Types
import GHC.Driver.Pipeline
import GHC.Driver.Phases
import GHC.Driver.Main
import GHC.Driver.Hooks
import GHC.Driver.CodeOutput

-- Stg Types
import FastString
import Module
import NameSet
import Stream (Stream)
import qualified Stream
import GHC.Stg.Syntax
import CostCentre
import GHC.Stg.Lint
import GHC.Stg.FVs
import GHC.Stg.Unarise

-- Core Passes
import GHC.StgToCmm (codeGen)
import GHC.Cmm
import GHC.Cmm.Info (cmmToRawCmm )
import UniqSupply ( mkSplitUniqSupply, initUs_ )

import Control.Monad.Trans
import Control.Monad

import Data.Binary
import qualified Data.Set as Set

import qualified Stg.Syntax as C
import qualified Stg.Convert as C

-------------------------------------------------------------------------------
-- Module
-------------------------------------------------------------------------------

modl :: Module
modl = mkModule mainUnitId (mkModuleName ":Main")

modloc :: ModLocation
modloc = ModLocation
 { ml_hs_file  = Nothing
 , ml_hi_file  = "Example.hi"
 , ml_obj_file = "Example.o"
 , ml_hie_file = "Example.hie"
 }

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

data Backend = NCG | LLVM

compileProgram :: Backend -> [TyCon] -> [StgTopBinding] -> IO ()
compileProgram backend tyCons topBinds_simple = runGhc (Just libdir) $ do
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

  -- Compile & Link
  dflags <- getSessionDynFlags
  pkgs <- setSessionDynFlags $
    dflags { hscTarget = target, ghcLink = LinkBinary }
    `gopt_set`  Opt_KeepSFiles
    `gopt_set`  Opt_KeepLlvmFiles
--    `dopt_set`  Opt_D_dump_cmm
--    `dopt_set`  Opt_D_dump_cmm_raw
--    `dopt_set`  Opt_D_dump_cmm_from_stg
    `dopt_set`  Opt_D_dump_timings
    `gopt_set`  Opt_DoStgLinting
    `gopt_set`  Opt_DoCmmLinting

  let libSet = Set.fromList ["rts", "ghc-prim-cbits", "base-cbits", "integer-gmp-cbits"]
  dflags <- getSessionDynFlags
  let ignored_pkgs  = [IgnorePackage p |  p <- map (unpackFS . installedUnitIdFS) pkgs, Set.notMember p libSet]
      my_pkgs       = [ExposePackage p (PackageArg p)  (ModRenaming True []) | p <- Set.toList libSet]
  setSessionDynFlags $ dflags { ignorePackageFlags = ignored_pkgs, packageFlags = my_pkgs }
  dflags <- getSessionDynFlags

  env <- getSession
  liftIO $ do
    newGen dflags env outFname modl tyCons ccs topBinds hpc
    oneShot env StopLn [(outFname, Just link){-, ("my_lib.o", Nothing)-}, ("stubs.c", Nothing)]
  pure ()

-------------
-- from GHC
-------------
cStub :: String
cStub = unlines
  [ "#include \"stdio.h\""
  , "HsInt32 ghczuwrapperZC0ZCbaseZCSystemziPosixziInternalsZCSEEKzuEND(void) {return SEEK_END;}"
  , "#include \"stdio.h\""
  , "HsInt32 ghczuwrapperZC1ZCbaseZCSystemziPosixziInternalsZCSEEKzuSET(void) {return SEEK_SET;}"
  , "#include \"stdio.h\""
  , "HsInt32 ghczuwrapperZC2ZCbaseZCSystemziPosixziInternalsZCSEEKzuCUR(void) {return SEEK_CUR;}"
  , "#include \"sys/stat.h\""
  , "HsInt32 ghczuwrapperZC3ZCbaseZCSystemziPosixziInternalsZCSzuISSOCK(HsWord32 a1) {return S_ISSOCK(a1);}"
  , "#include \"sys/stat.h\""
  , "HsInt32 ghczuwrapperZC4ZCbaseZCSystemziPosixziInternalsZCSzuISFIFO(HsWord32 a1) {return S_ISFIFO(a1);}"
  , "#include \"sys/stat.h\""
  , "HsInt32 ghczuwrapperZC5ZCbaseZCSystemziPosixziInternalsZCSzuISDIR(HsWord32 a1) {return S_ISDIR(a1);}"
  , "#include \"sys/stat.h\""
  , "HsInt32 ghczuwrapperZC6ZCbaseZCSystemziPosixziInternalsZCSzuISBLK(HsWord32 a1) {return S_ISBLK(a1);}"
  , "#include \"sys/stat.h\""
  , "HsInt32 ghczuwrapperZC7ZCbaseZCSystemziPosixziInternalsZCSzuISCHR(HsWord32 a1) {return S_ISCHR(a1);}"
  , "#include \"sys/stat.h\""
  , "HsInt32 ghczuwrapperZC8ZCbaseZCSystemziPosixziInternalsZCSzuISREG(HsWord32 a1) {return S_ISREG(a1);}"
  , "#include \"HsBase.h\""
  , "HsInt32 ghczuwrapperZC9ZCbaseZCSystemziPosixziInternalsZCtcsetattr(HsInt32 a1, HsInt32 a2, struct termios* a3) {return tcsetattr(a1, a2, a3);}"
  , "#include \"HsBase.h\""
  , "HsInt32 ghczuwrapperZC10ZCbaseZCSystemziPosixziInternalsZCtcgetattr(HsInt32 a1, struct termios* a2) {return tcgetattr(a1, a2);}"
  , "#include \"signal.h\""
  , "HsInt32 ghczuwrapperZC11ZCbaseZCSystemziPosixziInternalsZCsigprocmask(HsInt32 a1, sigset_t* a2, sigset_t* a3) {return sigprocmask(a1, a2, a3);}"
  , "#include \"signal.h\""
  , "HsInt32 ghczuwrapperZC12ZCbaseZCSystemziPosixziInternalsZCsigaddset(sigset_t* a1, HsInt32 a2) {return sigaddset(a1, a2);}"
  , "#include \"signal.h\""
  , "HsInt32 ghczuwrapperZC13ZCbaseZCSystemziPosixziInternalsZCsigemptyset(sigset_t* a1) {return sigemptyset(a1);}"
  , "#include \"HsBase.h\""
  , "HsInt32 ghczuwrapperZC14ZCbaseZCSystemziPosixziInternalsZCmkfifo(void* a1, HsWord32 a2) {return mkfifo(a1, a2);}"
  , "#include \"HsBase.h\""
  , "HsInt32 ghczuwrapperZC15ZCbaseZCSystemziPosixziInternalsZCfcntl(HsInt32 a1, HsInt32 a2, struct flock* a3) {return fcntl(a1, a2, a3);}"
  , "#include \"HsBase.h\""
  , "HsInt32 ghczuwrapperZC16ZCbaseZCSystemziPosixziInternalsZCfcntl(HsInt32 a1, HsInt32 a2, HsInt64 a3) {return fcntl(a1, a2, a3);}"
  , "#include \"HsBase.h\""
  , "HsInt32 ghczuwrapperZC17ZCbaseZCSystemziPosixziInternalsZCfcntl(HsInt32 a1, HsInt32 a2) {return fcntl(a1, a2);}"
  , "#include \"HsBase.h\""
  , "HsInt32 ghczuwrapperZC18ZCbaseZCSystemziPosixziInternalsZCutime(void* a1, struct utimbuf* a2) {return utime(a1, a2);}"
  , "#include \"HsBase.h\""
  , "HsInt64 ghczuwrapperZC19ZCbaseZCSystemziPosixziInternalsZCwrite(HsInt32 a1, HsWord8* a2, HsWord64 a3) {return write(a1, a2, a3);}"
  , "#include \"HsBase.h\""
  , "HsInt64 ghczuwrapperZC20ZCbaseZCSystemziPosixziInternalsZCwrite(HsInt32 a1, HsWord8* a2, HsWord64 a3) {return write(a1, a2, a3);}"
  , "#include \"HsBase.h\""
  , "HsInt64 ghczuwrapperZC21ZCbaseZCSystemziPosixziInternalsZCread(HsInt32 a1, HsWord8* a2, HsWord64 a3) {return read(a1, a2, a3);}"
  , "#include \"HsBase.h\""
  , "HsInt64 ghczuwrapperZC22ZCbaseZCSystemziPosixziInternalsZCread(HsInt32 a1, HsWord8* a2, HsWord64 a3) {return read(a1, a2, a3);}"
  , "#include \"unistd.h\""
  , "HsInt64 ghczuwrapperZC23ZCbaseZCSystemziPosixziInternalsZClseek(HsInt32 a1, HsInt64 a2, HsInt32 a3) {return lseek(a1, a2, a3);}"
  ]

newGen :: DynFlags
       -> HscEnv
       -> FilePath
       -> Module
       -> [TyCon]
       -> CollectedCCs
       -> [StgTopBinding]
       -> HpcInfo
       -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], NameSet)
newGen dflags hsc_env output_filename this_mod data_tycons cost_centre_info stg_binds hpc_info = do
  -- TODO: add these to parameters
  let location = modloc
      foreign_stubs = NoStubs-- empty (text cStub)
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
