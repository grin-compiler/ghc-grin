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
import Name
import Id
import Module
import Unique
import OccName
import Stream (Stream)
import qualified Stream
import StgSyn
import CostCentre
import CodeOutput

import PrimOp
import TysWiredIn
import Literal
import MkId

-- Core Passes
import StgCmm (codeGen)
import Cmm
import CmmInfo (cmmToRawCmm )
import CmmPipeline (cmmPipeline)
import CmmBuildInfoTables (emptySRT)
import UniqSupply ( mkSplitUniqSupply, initUs_ )

import System.IO
import Data.Time
import Control.Monad.Trans
import Control.Monad
import qualified Data.ByteString.Char8 as BS8

-------------------------------------------------------------------------------
-- Module
-------------------------------------------------------------------------------

mkName :: Int -> String -> Name
mkName i n = mkExternalName (mkUnique 'u' i) modl (mkOccName OccName.varName n) noSrcSpan

modl :: Module
modl = mkModule mainUnitId (mkModuleName ":Main")

modloc :: ModLocation
modloc = ModLocation
 { ml_hs_file  = Nothing
 , ml_hi_file  = "Example.hi"
 , ml_obj_file = "Example.o"
 }

t0 :: Type
t0 = intTy

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

main :: IO ()
main = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags

  setSessionDynFlags $ dflags { hscTarget = HscAsm, ghcLink = LinkBinary }

  dflags <- getSessionDynFlags
  env <- getSession

  setTargets
    [ Target
        { targetId = TargetModule (mkModuleName "Example")
        , targetAllowObjCode = True
        , targetContents = Nothing
        }
    ]

  let ccs       = ([], [])
      hpc       = emptyHpcInfo False
      tyCons    = []
      --idg0      = mkVanillaGlobal xn t0
      --idl0      = mkLocalId xn t0
      mkIdN i n  = mkVanillaGlobal (mkName i n) t0
      mkId i    = mkVanillaGlobal (mkName i $ 'x' : show i) t0
      topBinds  =
        [ StgTopStringLit (mkId 0) (BS8.pack "Hello!")
        , StgTopLifted $ StgNonRec (mkIdN 1 "main") $
            StgRhsClosure dontCareCCS stgSatOcc [] SingleEntry [voidArgId] $
              StgOpApp (StgPrimOp IntAddOp)
                [ StgLitArg $ mkMachInt dflags 1
                , StgLitArg $ mkMachInt dflags 2
                ] intTy
        ]
  liftIO $ do
    let outFname  = "out.asm"
    newGen dflags env outFname modl tyCons ccs topBinds hpc
    oneShot env StopLn [(outFname, Just $ As False)]
  pure ()


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
