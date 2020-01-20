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
import Type
import TyCon
import TysPrim

import UnariseStg
import UniqSupply (mkSplitUniqSupply)

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

{-
-- | As 'tyConDataCons_maybe', but returns the empty list of constructors if no
-- constructors could be found
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons_maybe :: TyCon -> Maybe [DataCon]
tyConSingleDataCon_maybe :: TyCon -> Maybe DataCon
tyConSingleDataCon :: TyCon -> DataCon

tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon :: Type -> TyCon


-}
getDataCon :: Type -> DataCon
getDataCon = tyConSingleDataCon . tyConAppTyCon

-- CASE: unboxed tuple immediate decomposition
main2 = do
  let dflags    = unsafeGlobalDynFlags
      --idl0      = mkLocalId xn t0
      mkIdN i n = mkVanillaGlobal (mkName i n) t0
      mkId i    = mkVanillaGlobal (mkName i $ 'x' : show i) t0
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      --mkIdL i   = mkLocalId (mkNameL i $ 'l' : show i) t0
      idStr0    = mkId 0
      --u1IntTy   = mkTupleTy Unboxed [anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep]
      u2IntTy   = mkTupleTy Unboxed [anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep, anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep]
      idInt0    = mkIdT 100 u2IntTy
      idInt1    = mkId 200
      idInt2    = mkId 202
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!\n1 + 2 = %d\n")
        , StgTopLifted $ StgNonRec (mkIdN 1 "main") $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgConApp (getDataCon u2IntTy)
                  [ StgLitArg $ mkMachInt dflags 3
                  , StgLitArg $ mkMachInt dflags 4
                  ] []
              ) idInt0 (MultiValAlt 2)
              [ (DataAlt (getDataCon u2IntTy), [idInt1, idInt2],
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
                    , StgVarArg idInt2
                    ] intTy
                )
              ]
        ]
  compileProgram NCG topBinds

-- CASE: unboxed tuple stored in a single variable for later decomposition
main3 = do
  let dflags    = unsafeGlobalDynFlags
      --idl0      = mkLocalId xn t0
      mkIdN i n = mkVanillaGlobal (mkName i n) t0
      mkId i    = mkVanillaGlobal (mkName i $ 'x' : show i) t0
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      --mkIdL i   = mkLocalId (mkNameL i $ 'l' : show i) t0
      idStr0    = mkId 0
      --u1IntTy   = mkTupleTy Unboxed [anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep]
      u2IntTy   = mkTupleTy Unboxed [anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep, anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep]
      idInt0    = mkIdT 100 u2IntTy
      idInt01   = mkIdT 101 u2IntTy
      idInt1    = mkId 200
      idInt2    = mkId 202
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!\n1 + 2 = %d\n")
        , StgTopLifted $ StgNonRec (mkIdN 1 "main") $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgConApp (tupleDataCon Unboxed 2)
                  [ StgLitArg $ mkMachInt dflags 3
                  , StgLitArg $ mkMachInt dflags 4
                  ] []
              ) idInt0 (MultiValAlt 2)
              [ (DEFAULT, [],

                  StgCase (StgApp idInt0 []) idInt01 (MultiValAlt 2)

                    [ (DataAlt (getDataCon u2IntTy), [idInt1, idInt2],
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
                          , StgVarArg idInt1
                          ] intTy
                      )
                    ]
                )
              ]
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram NCG $ unarise us topBinds

repTy :: PrimRep -> Type
repTy = anyTypeOfKind . tYPE . primRepToRuntimeRep

-- CASE: pattern match on Lifted Boxed Tuple
main4 = do
  putStrLn "CASE: pattern match on Lifted Boxed Tuple"
  let dflags    = unsafeGlobalDynFlags
      --idl0      = mkLocalId xn t0
      mkIdN i n = mkVanillaGlobal (mkName i n) t0
      mkId i    = mkVanillaGlobal (mkName i $ 'x' : show i) t0
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      --mkIdL i   = mkLocalId (mkNameL i $ 'l' : show i) t0
      idStr0    = mkIdT 0 (repTy AddrRep)
      idStr1    = mkIdT 1 (repTy AddrRep)
      --u1IntTy   = mkTupleTy Unboxed [anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep]
      --u2IntTy   = mkTupleTy Unboxed [repTy IntRep, repTy IntRep]
      b2IntTy   = mkTupleTy Boxed [repTy IntRep, repTy IntRep]
      idInt0    = mkIdT 100 (repTy LiftedRep)
      idInt01   = mkIdT 101 (repTy LiftedRep)
      idInt1    = mkId 200
      idInt2    = mkId 202
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Value: %d\n")
        , StgTopStringLit idStr1 (BS8.pack "Value: %d\n")
        , StgTopLifted $ StgNonRec (mkIdN 1 "main") $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgConApp (tupleDataCon Boxed 2)
                  [ StgLitArg $ mkMachInt dflags 3
                  , StgLitArg $ mkMachInt dflags 4
                  ] []
              ) idInt0 PolyAlt
              [ (DEFAULT, [],

                  StgCase (StgApp idInt0 []) idInt01 (AlgAlt $ tyConAppTyCon b2IntTy)

                    [ (DataAlt (getDataCon b2IntTy), [idInt1, idInt2],
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
                          , StgVarArg idInt2
                          ] intTy
                      )
                    ]
                )
              ]
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram NCG $ {-unarise us-} topBinds

-- CASE: pattern match on Unlifted Boxed Tuple
main5 = do
  putStrLn "CASE: pattern match on Unlifted Boxed Tuple"
  let dflags    = unsafeGlobalDynFlags
      --idl0      = mkLocalId xn t0
      mkIdN i n = mkVanillaGlobal (mkName i n) t0
      mkId i    = mkVanillaGlobal (mkName i $ 'x' : show i) t0
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      --mkIdL i   = mkLocalId (mkNameL i $ 'l' : show i) t0
      idStr0    = mkIdT 0 (repTy AddrRep)
      idStr1    = mkIdT 1 (repTy AddrRep)
      --u1IntTy   = mkTupleTy Unboxed [anyTypeOfKind . tYPE . primRepToRuntimeRep $ IntRep]
      --u2IntTy   = mkTupleTy Unboxed [repTy IntRep, repTy IntRep]
      b2IntTy   = mkTupleTy Boxed [repTy IntRep, repTy IntRep]
      idInt0    = mkIdT 100 (repTy UnliftedRep)
      idInt01   = mkIdT 101 (repTy UnliftedRep)
      idInt1    = mkId 200
      idInt2    = mkId 202
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Value: %d\n")
        , StgTopStringLit idStr1 (BS8.pack "Value: %d\n")
        , StgTopLifted $ StgNonRec (mkIdN 1 "main") $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgConApp (tupleDataCon Boxed 2)
                  [ StgLitArg $ mkMachInt dflags 3
                  , StgLitArg $ mkMachInt dflags 4
                  ] []
              ) idInt0 PolyAlt
              [ (DEFAULT, [],

                  StgCase (StgApp idInt0 []) idInt01 (AlgAlt $ tyConAppTyCon b2IntTy)

                    [ (DataAlt (getDataCon b2IntTy), [idInt1, idInt2],
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
                          , StgVarArg idInt2
                          ] intTy
                      )
                    ]
                )
              ]
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram NCG $ unarise us topBinds

-- CASE: user ADT Lifted
{-
mkTupleTyCon :: Name
             -> [TyConBinder]
             -> Kind    -- ^ Result kind of the 'TyCon'
             -> Arity   -- ^ Arity of the tuple 'TyCon'
             -> DataCon
             -> TupleSort    -- ^ Whether the tuple is boxed or unboxed
             -> AlgTyConFlav
             -> TyCon

mkAlgTyCon :: Name
           -> [TyConBinder]  -- ^ Binders of the 'TyCon'
           -> Kind              -- ^ Result kind
           -> [Role]            -- ^ The roles for each TyVar
           -> Maybe CType       -- ^ The C type this type corresponds to
                                --   when using the CAPI FFI
           -> [PredType]        -- ^ Stupid theta: see 'algTcStupidTheta'
           -> AlgTyConRhs       -- ^ Information about data constructors
           -> AlgTyConFlav      -- ^ What flavour is it?
                                -- (e.g. vanilla, type family)
           -> Bool              -- ^ Was the 'TyCon' declared with GADT syntax?
           -> TyCon

mkDataTyConRhs :: [DataCon] -> AlgTyConRhs

data AlgTyConFlav
  = -- | An ordinary type constructor has no parent.
    VanillaAlgTyCon
       TyConRepName

type TyConRepName = Name -- The Name of the top-level declaration
type Kind = Type


-- DataCon

-- | Build a new data constructor
mkDataCon :: Name
          -> Bool               -- ^ Is the constructor declared infix?
          -> TyConRepName       -- ^  TyConRepName for the promoted TyCon
          -> [HsSrcBang]        -- ^ Strictness/unpack annotations, from user
          -> [FieldLabel]       -- ^ Field labels for the constructor,
                                -- if it is a record, otherwise empty
          -> [TyVar]            -- ^ Universals.
          -> [TyVar]            -- ^ Existentials.
          -> [TyVarBinder]      -- ^ User-written 'TyVarBinder's.
                                --   These must be Inferred/Specified.
                                --   See @Note [TyVarBinders in DataCons]@
          -> [EqSpec]           -- ^ GADT equalities
          -> KnotTied ThetaType -- ^ Theta-type occuring before the arguments proper
          -> [KnotTied Type]    -- ^ Original argument types
          -> KnotTied Type      -- ^ Original result type
          -> RuntimeRepInfo     -- ^ See comments on 'TyCon.RuntimeRepInfo'
          -> KnotTied TyCon     -- ^ Representation type constructor
          -> ConTag             -- ^ Constructor tag
          -> ThetaType          -- ^ The "stupid theta", context of the data
                                -- declaration e.g. @data Eq a => T a ...@
          -> Id                 -- ^ Worker Id
          -> DataConRep         -- ^ Representation
          -> DataCon
  -- Can get the tag from the TyCon

-}
{-
  case TyCon used:
    isEnumerationTyCon
    tagToClosure
    tyConFamilySize
-}


-- CASE: user ADT Unlifted
-- CASE: user ADT with arguments Lifted
-- CASE: user ADT with arguments Unlifted

{-
  QUESTIONS:
    - can case bind unboxed tuples to variables for later pattern matching/decomposition?
      A: yes, but requires 'unarise' STG pass before codegen

    - which one matter for the codegen, the DataCon tag value or the TyCon's AlgTyConRhs [DataCon] order?

-}