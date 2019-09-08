module StgLoopback where

-- Compiler
import GHC
import DynFlags
import ErrUtils
import Platform ( platformOS, osSubsectionsViaSymbols )
import HscMain
import HscTypes
import Outputable
import GHC.Paths ( libdir )

-- Core Types
import Var
import Name
import Kind
import Avail
import IdInfo
import Module
--import TypeRep
import Unique
import OccName
import InstEnv
import NameSet
import RdrName
import FamInstEnv
import Stream (Stream)
import qualified Stream
import qualified CoreSyn as Syn
import StgSyn
import CostCentre
import CodeOutput

-- Core Passes
import StgCmm (codeGen)
import Cmm
import CmmInfo (cmmToRawCmm )
import CmmLint (cmmLint)
import CmmPipeline (cmmPipeline)
import CmmBuildInfoTables (emptySRT) -- , srtToData)
import AsmCodeGen ( nativeCodeGen )
import UniqSupply ( mkSplitUniqSupply, initUs_ )

import System.IO
import Data.Time
import Control.Monad.Trans
import Control.Monad

-------------------------------------------------------------------------------
-- Module
-------------------------------------------------------------------------------

mkName :: Int -> String -> Name
mkName i n = mkInternalName (mkUnique 'n' i) (mkOccName OccName.varName n) noSrcSpan

xn :: Name
xn = mkName 0 "x"

an :: Name
an = mkName 1 "a"

fn :: Name
fn = mkExternalName (mkUnique 'n' 2) modl (mkOccName OccName.varName "f") noSrcSpan
{-
-- a :: *
a :: TyVar
a = mkTyVar an undefined--anyKind

-- x :: a
x :: Var
x = mkLocalVar VanillaId xn (TyVarTy a) vanillaIdInfo

-- f :: a -> a
fv :: Var
fv = mkGlobalVar VanillaId fn (FunTy (TyVarTy a) (TyVarTy a)) vanillaIdInfo

def :: [Syn.CoreBind]
def = [Syn.NonRec fv f]

f :: Syn.Expr Var
f = Syn.Lam x (Syn.Var x)
-}
modl :: Module
modl = mkModule mainUnitId (mkModuleName "Example")
{-
guts :: ModGuts
guts = ModGuts
  {
      mg_module          = modl,
      mg_exports         = [Avail fn],
      mg_deps            = noDependencies,
      mg_dir_imps        = emptyModuleEnv,
      mg_used_names      = mkNameSet [fn],
      mg_used_th         = False,
      mg_rdr_env         = emptyGlobalRdrEnv,
      mg_fix_env         = emptyFixityEnv,
      mg_tcs             = [],
      mg_insts           = [],
      mg_fam_insts       = [],
      mg_patsyns         = [],
      mg_rules           = [],
      mg_binds           = def,
      mg_foreign         = NoStubs,
      mg_warns           = NoWarnings,
      mg_hpc_info        = NoHpcInfo False,
      mg_modBreaks       = emptyModBreaks,
      mg_vect_decls      = [],
      mg_vect_info       = noVectInfo,
      mg_boot            = False,
      mg_anns            = [],
      mg_inst_env        = emptyInstEnv,
      mg_fam_inst_env    = emptyFamInstEnv,
      mg_safe_haskell    = Sf_None,
      mg_trust_pkg       = False,
      mg_dependent_files = []
  }

summ :: DynFlags -> ModSummary
summ dflags = ModSummary 
  {
      ms_mod          = modl,
      ms_hsc_src      = HsSrcFile,
      ms_location     = ModLocation {
          ml_hs_file  = Nothing
      ,   ml_hi_file  = "Example.hi"
      ,   ml_obj_file = "Example.o"
      },
      ms_hs_date      = UTCTime (toEnum 0) 0,
      ms_obj_date     = Nothing,
      ms_iface_date   = Nothing,
      ms_srcimps      = [],
      ms_textual_imps = [],
      ms_hspp_file    = "Example.hs",
      ms_hspp_opts    = dflags,
      ms_hspp_buf     = Nothing
  }
-}
modloc :: ModLocation
modloc = ModLocation 
 { ml_hs_file  = Nothing
 , ml_hi_file  = "Example.hi"
 , ml_obj_file = "Example.o"
 }

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

  setTargets [Target
    { targetId = TargetModule (mkModuleName "Example")
    , targetAllowObjCode = True
    , targetContents = Nothing }]
{-
  -- Run the Core prep pass
  prep <- liftIO $ corePrepPgm env (ms_location (summ dflags)) (mg_binds guts) (mg_tcs guts)

  -- Transform Core into STG
  stg <- liftIO $ coreToStg dflags (mg_module guts) prep

  -- STG Transformer
  (stg_binds2, cost_centre_info) <- liftIO $ stg2stg dflags (mg_module guts) stg
-}
  let stg_binds2 = []
      cost_centre_info = ([],[])
  -- Generated Cmm
  let cmms = codeGen dflags modl [] cost_centre_info stg_binds2 (NoHpcInfo False)

  -- Initialize a name supply for the Cmm pipeline
  us <- liftIO $ mkSplitUniqSupply 'S'
  let initTopSRT = initUs_ us --emptySRT
      --run_pipeline = cmmPipeline env
      run_pipeline us cmmgroup = do
        (_topSRT, cmmgroup) <-
          cmmPipeline env (emptySRT modl) cmmgroup
        return (us, cmmgroup)

{-
        let -- Make up a module name to give the NCG. We can't pass bottom here
            -- lest we reproduce #11784.
            mod_name = mkModuleName $ "Cmm$" ++ FilePath.takeFileName filename
            cmm_mod = mkModule (thisPackage dflags) mod_name
        (_, cmmgroup) <- cmmPipeline hsc_env (emptySRT cmm_mod) cmm
        rawCmms <- cmmToRawCmm dflags (Stream.yield cmmgroup)

          let run_pipeline us cmmgroup = do
                (_topSRT, cmmgroup) <-
                  cmmPipeline hsc_env (emptySRT this_mod) cmmgroup
                return (us, cmmgroup)

          in do _ <- Stream.mapAccumL run_pipeline us ppr_stream1
                return ()
-}

  -- Collect the Cmm code stream after running the pipeline.
  let cmmstream = do
        undefined
{-
       a <- Stream.mapAccumL run_pipeline us cmms
       Stream.yield (a)
-}
  -- Prepare the Cmm for
  genraw <- liftIO $ cmmToRawCmm dflags cmmstream

  -- Initialize name supply for the native code generator and generate x86 to a
  -- file from the prepared Cmm.
  ncg_uniqs <- liftIO $ mkSplitUniqSupply 'n'
  fname <- liftIO $ (openFile "Example.asm" WriteMode)
  liftIO $ nativeCodeGen dflags modl modloc fname ncg_uniqs genraw
{-
  -- Dump the outputted Stg and  Cmm out
  gen <- liftIO $ Stream.collect cmmstream
  --liftIO $ putStrLn "=== STG ==="
  --liftIO $ putStrLn $ showGhc stg_binds2

  liftIO $ putStrLn "=== CMM ==="
  liftIO $ putStrLn $ showGhc gen
-}
  pure ()


newGen dflags hsc_env this_mod data_tycons cost_centre_info hpc_info = do
  -- TODO
  let stg_binds = []
      output_filename = ""
      location = modloc
      foreign_stubs = undefined
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
