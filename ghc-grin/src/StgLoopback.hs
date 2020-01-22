module StgLoopback where

-- Compiler
import GHC
import DynFlags
import ErrUtils
import Platform ( platformOS, osSubsectionsViaSymbols )
import HscTypes
import Outputable
import GHC.Paths ( libdir )
import DriverPipeline
import DriverPhases

-- Stg Types
import Module
import Stream (Stream)
import qualified Stream
import StgSyn
import CostCentre
import CodeOutput
import StgLint

-- Core Passes
import StgCmm (codeGen)
import Cmm
import CmmInfo (cmmToRawCmm )
import CmmPipeline (cmmPipeline)
import CmmBuildInfoTables (emptySRT)
import UniqSupply ( mkSplitUniqSupply, initUs_ )

import Control.Monad.Trans
import Control.Monad

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
 }

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

data Backend = NCG | LLVM

compileProgram :: Backend -> [TyCon] -> [StgTopBinding] -> IO ()
compileProgram backend tyCons topBinds = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags

  liftIO $ do
    putStrLn "==== STG ===="
    putStrLn $ showSDoc dflags $ pprStgTopBindings topBinds
    putStrLn "==== Lint STG ===="
    lintStgTopBindings dflags True "Manual" topBinds

  -- construct STG program manually
  let ccs       = ([], [])
      hpc       = emptyHpcInfo False

  -- backend
  let
    outFname  = "out.ll"

    (target, link) = case backend of
      LLVM  -> (HscLlvm, LlvmOpt)
      NCG   -> (HscAsm, As False)

  -- Compile & Link
  dflags <- getSessionDynFlags
  setSessionDynFlags $
    dflags { hscTarget = target, ghcLink = LinkBinary }
    `gopt_set`  Opt_KeepSFiles
    `gopt_set`  Opt_KeepLlvmFiles
--    `dopt_set`  Opt_D_dump_cmm
    `dopt_set`  Opt_D_dump_cmm_raw
--    `dopt_set`  Opt_D_dump_cmm_from_stg
    `dopt_set`  Opt_D_dump_timings
    `gopt_set`  Opt_DoStgLinting
    `gopt_set`  Opt_DoCmmLinting

  dflags <- getSessionDynFlags

  env <- getSession
  liftIO $ do
    newGen dflags env outFname modl tyCons ccs topBinds hpc
    oneShot env StopLn [(outFname, Just link)]
  pure ()
{-
  TODO:
    prevent linking haskell libraries i.e. base, integer-gmp, ghc-prim
-}

-------------
-- from GHC
-------------

newGen :: DynFlags
       -> HscEnv
       -> FilePath
       -> Module
       -> [TyCon]
       -> CollectedCCs
       -> [StgTopBinding]
       -> HpcInfo
       -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)])
newGen dflags hsc_env output_filename this_mod data_tycons cost_centre_info stg_binds hpc_info = do
  -- TODO: add these to parameters
  let location = modloc
      foreign_stubs = NoStubs
      foreign_files = []
      dependencies = []

  cmms <- {-# SCC "StgCmm" #-}
                  doCodeGen hsc_env this_mod data_tycons
                      cost_centre_info
                      stg_binds hpc_info

  ------------------  Code output -----------------------
  rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
            cmmToRawCmm dflags cmms

  let dump a = do dumpIfSet_dyn dflags Opt_D_dump_cmm_raw "Raw Cmm"
                    (ppr a)
                  return a
      rawcmms1 = Stream.mapM dump rawcmms0

  (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps)
      <- {-# SCC "codeOutput" #-}
        codeOutput dflags this_mod output_filename location
        foreign_stubs foreign_files dependencies rawcmms1
  return (output_filename, stub_c_exists, foreign_fps)


doCodeGen   :: HscEnv -> Module -> [TyCon]
            -> CollectedCCs
            -> [StgTopBinding]
            -> HpcInfo
            -> IO (Stream IO CmmGroup ())
         -- Note we produce a 'Stream' of CmmGroups, so that the
         -- backend can be run incrementally.  Otherwise it generates all
         -- the C-- up front, which has a significant space cost.
doCodeGen hsc_env this_mod data_tycons
              cost_centre_info stg_binds hpc_info = do
    let dflags = hsc_dflags hsc_env

    let cmm_stream :: Stream IO CmmGroup ()
        cmm_stream = {-# SCC "StgCmm" #-}
            StgCmm.codeGen dflags this_mod data_tycons
                           cost_centre_info stg_binds hpc_info

        -- codegen consumes a stream of CmmGroup, and produces a new
        -- stream of CmmGroup (not necessarily synchronised: one
        -- CmmGroup on input may produce many CmmGroups on output due
        -- to proc-point splitting).

    let dump1 a = do dumpIfSet_dyn dflags Opt_D_dump_cmm_from_stg
                       "Cmm produced by codegen" (ppr a)
                     return a

        ppr_stream1 = Stream.mapM dump1 cmm_stream

    -- We are building a single SRT for the entire module, so
    -- we must thread it through all the procedures as we cps-convert them.
    us <- mkSplitUniqSupply 'S'

    -- When splitting, we generate one SRT per split chunk, otherwise
    -- we generate one SRT for the whole module.
    let
     pipeline_stream
      | gopt Opt_SplitObjs dflags || gopt Opt_SplitSections dflags ||
        osSubsectionsViaSymbols (platformOS (targetPlatform dflags))
        = {-# SCC "cmmPipeline" #-}
          let run_pipeline us cmmgroup = do
                (_topSRT, cmmgroup) <-
                  cmmPipeline hsc_env (emptySRT this_mod) cmmgroup
                return (us, cmmgroup)

          in do _ <- Stream.mapAccumL run_pipeline us ppr_stream1
                return ()

      | otherwise
        = {-# SCC "cmmPipeline" #-}
          let run_pipeline = cmmPipeline hsc_env
          in void $ Stream.mapAccumL run_pipeline (emptySRT this_mod) ppr_stream1

    let
        dump2 a = do dumpIfSet_dyn dflags Opt_D_dump_cmm
                        "Output Cmm" (ppr a)
                     return a

        ppr_stream2 = Stream.mapM dump2 pipeline_stream

    return ppr_stream2
