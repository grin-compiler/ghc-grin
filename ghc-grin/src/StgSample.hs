module StgSample where

import StgLoopback

-- Compiler
import GHC
import DynFlags
import Outputable

-- Stg Types
import Name
import Id
import Unique
import OccName
import StgSyn
import CostCentre
import ForeignCall
import FastString
import BasicTypes
import CoreSyn (AltCon(..))

import PrimOp
import TysWiredIn
import Literal
import MkId
import TyCon

import qualified Data.ByteString.Char8 as BS8

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

mkName :: Int -> String -> Name
mkName i n = mkExternalName (mkUnique 'u' i) modl (mkOccName OccName.varName n) noSrcSpan

--mkNameL :: Int -> String -> Name
--mkNameL i n = mkInternalName (mkUnique 'u' i) (mkOccName OccName.varName n) noSrcSpan


t0 :: Type
t0 = intTy

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-------------------------------------------------------------------------------
-- Sample STG program
-------------------------------------------------------------------------------

{-
  TODO:
    - data constructor creation
    pattern match
      - without parameters
      - with parameters
      - datacon
      - unboxed tuple
    unboxed tuple
      - return value
      - parameter
      - bit pattern
      - pointer

HINT
  id    = unique id
  name  = unique id + occ name

CONVENTION:
  always use unique occ names

-}

{-
do
  str_ "str.0" "Hello!\n1 + 2 = %d\n"
  "str.0" := "Hello!\n1 + 2 = %d\n"
  id_ "str.0" := "Hello!\n1 + 2 = %d\n"
  rec_
    "main" := 
-}

{-
  unique name spaces in GHC
    0123456789BCDEHLPRSXcdgikmstvz

  used by the stg builder
    fu
-}

main = do
  let dflags    = unsafeGlobalDynFlags
      --idl0      = mkLocalId xn t0
      mkIdN i n = mkVanillaGlobal (mkName i n) t0
      mkId i    = mkVanillaGlobal (mkName i $ 'x' : show i) t0
      --mkIdL i   = mkLocalId (mkNameL i $ 'l' : show i) t0
      idStr0    = mkId 0
      idInt0    = mkId 100
      idInt1    = mkId 200
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!\n1 + 2 = %d\n")
        , StgTopLifted $ StgNonRec (mkIdN 1 "main") $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgOpApp (StgPrimOp IntAddOp)
                  [ StgLitArg $ mkMachInt dflags 1
                  , StgLitArg $ mkMachInt dflags 2
                  ] intTy
              ) idInt0 (PrimAlt IntRep)
              [ (DEFAULT, [],
                  StgOpApp
                    (StgFCallOp
                      (CCall $ CCallSpec
                        (StaticTarget NoSourceText (mkFastString "printf") Nothing True)
                        CCallConv
                        PlayRisky
                      )
                      (mkUnique 'f' 0)
                    )
                    [ StgVarArg idStr0
                    , StgVarArg idInt0
                    ] intTy
                )
              ]
        ]
  compileProgram NCG topBinds
