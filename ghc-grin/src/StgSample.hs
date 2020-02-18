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
import DataCon

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
{-
data PrimRep
  = VoidRep
  | LiftedRep
  | UnliftedRep   -- ^ Unlifted pointer
  | Int64Rep      -- ^ Signed, 64 bit value (with 32-bit words only)
  | Word64Rep     -- ^ Unsigned, 64 bit value (with 32-bit words only)
  | AddrRep       -- ^ A pointer, but /not/ to a Haskell value (use '(Un)liftedRep')
  | FloatRep
  | DoubleRep
  deriving (Generic, Data, Eq, Show, Ord)

data RepType
  = SingleValue   PrimRep
  | UnboxedTuple  [PrimRep]
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
  compileProgram NCG [] topBinds

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
  compileProgram NCG [] topBinds

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

  compileProgram NCG [] $ unarise us topBinds

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

  compileProgram NCG [] $ {-unarise us-} topBinds

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

  compileProgram NCG [] $ unarise us topBinds

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

simpleDataCon :: TyCon -> Name -> [PrimRep] -> ConTag -> DataCon
simpleDataCon tc name args tag = mkDataCon
  name False (error "TyConRepName") [] [] [] [] [] [] []
  (map repTy args) (error "Original result type") (error "RuntimeRepInfo")
  tc tag [] fakeWorkerId NoDataConRep
  where
    fakeWorkerId = mkIdNT 666 "fakeWokerId" (error "repTy LiftedRep")

simpleTyCon :: Name -> [DataCon] -> TyCon
simpleTyCon name dataCons = mkAlgTyCon name [] (error "Kind") [] Nothing [] (mkDataTyConRhs dataCons) (VanillaAlgTyCon (error "TyConRepName")) False

mkIdNT i n t  = mkVanillaGlobal (mkName i n) t

main6 = do
  putStrLn "CASE: user ADT Lifted"
  let dflags      = unsafeGlobalDynFlags
      mkIdT i t   = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0      = mkIdT 0 (repTy AddrRep)
      idStr1      = mkIdT 1 (repTy AddrRep)
      idLifted0   = mkIdT 100 (repTy LiftedRep)
      idLifted01  = mkIdT 101 (repTy LiftedRep)
      dcMyFalse   = simpleDataCon tcMyBool (mkName 9001 "MyFalse") [] 2
      dcMyTrue    = simpleDataCon tcMyBool (mkName 9002 "MyTrue")  [] 1
      tcMyBool    = simpleTyCon (mkName 8001 "MyBool") [dcMyFalse, dcMyTrue]
      tyMyBool    = mkTyConApp tcMyBool []
      topBinds    =
        [ StgTopStringLit idStr0 (BS8.pack "Value: MyFalse\n")
        , StgTopStringLit idStr1 (BS8.pack "Value: MyTrue\n")
        , StgTopLifted $ StgNonRec (mkVanillaGlobal (dataConName dcMyFalse) (repTy LiftedRep)) $ StgRhsCon dontCareCCS dcMyFalse []
        , StgTopLifted $ StgNonRec (mkVanillaGlobal (dataConName dcMyTrue)  (repTy LiftedRep)) $ StgRhsCon dontCareCCS dcMyTrue []
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgConApp dcMyFalse [] (error "StgConApp type list")
              ) idLifted0 PolyAlt
              [ (DEFAULT, [],

                  StgCase (StgApp idLifted0 []) idLifted01 (AlgAlt tcMyBool)

                    [ (DataAlt (dcMyTrue), [],
                        StgOpApp
                          (StgFCallOp
                            (CCall $ CCallSpec
                              (StaticTarget NoSourceText (mkFastString "printf") Nothing True)
                              CCallConv
                              PlayRisky
                            )
                            (mkUnique 'f' 0)
                          )
                          [ StgVarArg idStr1
                          ] intTy
                      )
                    , (DataAlt (dcMyFalse), [],
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
                          ] intTy
                      )
                    ]

                )
              ]
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram NCG [tcMyBool] $ {-unarise us-} topBinds


-- CASE: user ADT Unlifted
main7 = do
  putStrLn "CASE: user ADT Unlifted"
  let dflags      = unsafeGlobalDynFlags
      mkIdT i t   = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0      = mkIdT 0 (repTy AddrRep)
      idStr1      = mkIdT 1 (repTy AddrRep)
      idUnlifted0   = mkIdT 100 (repTy UnliftedRep)
      idUnlifted01  = mkIdT 101 (repTy UnliftedRep)
      dcMyFalse   = simpleDataCon tcMyBool (mkName 9001 "MyFalse") [] 1
      dcMyTrue    = simpleDataCon tcMyBool (mkName 9002 "MyTrue")  [] 2
      tcMyBool    = simpleTyCon (mkName 8001 "MyBool") [dcMyFalse, dcMyTrue]
      tyMyBool    = mkTyConApp tcMyBool []
      topBinds    =
        [ StgTopStringLit idStr0 (BS8.pack "Value: MyFalse\n")
        , StgTopStringLit idStr1 (BS8.pack "Value: MyTrue\n")
        , StgTopLifted $ StgNonRec (mkVanillaGlobal (dataConName dcMyFalse) (repTy LiftedRep)) $ StgRhsCon dontCareCCS dcMyFalse []
        , StgTopLifted $ StgNonRec (mkVanillaGlobal (dataConName dcMyTrue)  (repTy LiftedRep)) $ StgRhsCon dontCareCCS dcMyTrue []
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgConApp dcMyTrue [] []
              ) idUnlifted0 PolyAlt
              [ (DEFAULT, [],

                  StgCase (StgApp idUnlifted0 []) idUnlifted01 (AlgAlt tcMyBool)

                    [ (DataAlt (dcMyTrue), [],
                        StgOpApp
                          (StgFCallOp
                            (CCall $ CCallSpec
                              (StaticTarget NoSourceText (mkFastString "printf") Nothing True)
                              CCallConv
                              PlayRisky
                            )
                            (mkUnique 'f' 0)
                          )
                          [ StgVarArg idStr1
                          ] intTy
                      )
                    , (DataAlt (dcMyFalse), [],
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
                          ] intTy
                      )
                    ]

                )
              ]
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram LLVM [tcMyBool] $ {-unarise us-} topBinds

-- CASE: user ADT with arguments Lifted
sampleADTArgFloat = do
  putStrLn "CASE: user ADT with arguments Lifted"
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      idStr1    = mkIdT 1 (repTy AddrRep)
      id0       = mkIdT 100 (repTy LiftedRep)
      id01      = mkIdT 101 (repTy LiftedRep)
      dcMyConA  = simpleDataCon tcMyADT (mkName 9001 "MyConA") [IntRep, IntRep] 1
      dcMyConB  = simpleDataCon tcMyADT (mkName 9002 "MyConB") [FloatRep] 2
      tcMyADT   = simpleTyCon (mkName 8001 "MyADT") [dcMyConA, dcMyConB]
      tyMyADT   = mkTyConApp tcMyADT []
      idInt1    = mkIdT 200 (repTy IntRep)
      idInt2    = mkIdT 202 (repTy IntRep)
      id3_f32  = mkIdT 203 (repTy FloatRep)
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Value: MyConA %d %d\n")
        , StgTopStringLit idStr1 (BS8.pack "Value: MyConB %f\n")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (

                StgConApp dcMyConB
                  [ StgLitArg $ mkMachFloat 3.14
                  ] []

              ) id0 PolyAlt
              [ (DEFAULT, [],

                  StgCase (StgApp id0 []) id01 (AlgAlt tcMyADT)

                    [ (DataAlt (dcMyConA), [idInt1, idInt2],
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
                          , StgVarArg idInt2
                          ] intTy
                      )
                    , (DataAlt (dcMyConB), [id3_f32],
                        StgOpApp
                          (StgFCallOp
                            (CCall $ CCallSpec
                              (StaticTarget NoSourceText (mkFastString "printf") Nothing True)
                              CCallConv
                              PlayRisky
                            )
                            (mkUnique 'f' 0)
                          )
                          [ StgVarArg idStr1
                          , StgVarArg id3_f32
                          ] intTy
                      )
                    ]

                )
              ]
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram LLVM [tcMyADT] $ {-unarise us-} topBinds

sampleADTArgDouble = do
  putStrLn "CASE: user ADT with arguments Lifted"
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      idStr1    = mkIdT 1 (repTy AddrRep)
      id0       = mkIdT 100 (repTy LiftedRep)
      id01      = mkIdT 101 (repTy LiftedRep)
      dcMyConA  = simpleDataCon tcMyADT (mkName 9001 "MyConA") [IntRep, IntRep] 1
      dcMyConB  = simpleDataCon tcMyADT (mkName 9002 "MyConB") [DoubleRep] 2
      tcMyADT   = simpleTyCon (mkName 8001 "MyADT") [dcMyConA, dcMyConB]
      tyMyADT   = mkTyConApp tcMyADT []
      idInt1    = mkIdT 200 (repTy IntRep)
      idInt2    = mkIdT 202 (repTy IntRep)
      id3_f64  = mkIdT 203 (repTy DoubleRep)
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Value: MyConA %d %d\n")
        , StgTopStringLit idStr1 (BS8.pack "Value: MyConB %lf\n")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (

                StgConApp dcMyConB
                  [ StgLitArg $ mkMachDouble 3.14
                  ] []

              ) id0 PolyAlt
              [ (DEFAULT, [],

                  StgCase (StgApp id0 []) id01 (AlgAlt tcMyADT)

                    [ (DataAlt (dcMyConA), [idInt1, idInt2],
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
                          , StgVarArg idInt2
                          ] intTy
                      )
                    , (DataAlt (dcMyConB), [id3_f64],
                        StgOpApp
                          (StgFCallOp
                            (CCall $ CCallSpec
                              (StaticTarget NoSourceText (mkFastString "printf") Nothing True)
                              CCallConv
                              PlayRisky
                            )
                            (mkUnique 'f' 0)
                          )
                          [ StgVarArg idStr1
                          , StgVarArg id3_f64
                          ] intTy
                      )
                    ]

                )
              ]
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram LLVM [tcMyADT] $ {-unarise us-} topBinds

-- CASE: user ADT with arguments Unlifted

{-
  QUESTIONS:
    - can case bind unboxed tuples to variables for later pattern matching/decomposition?
      A: yes, but requires 'unarise' STG pass before codegen

    - which one matter for the codegen, the DataCon tag value or the TyCon's AlgTyConRhs [DataCon] order?
      A: only Tag value matters (starting from 1), no order matters (i.e. Case Alt, AlgTyConRhs [DataCon])

-}

-- CASE: FFI float argument
sampleFFIArgFloat = do
  putStrLn "CASE: FFI with float argument"
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      topBinds  =
        [ StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgOpApp
                (StgFCallOp
                  (CCall $ CCallSpec
                    (StaticTarget NoSourceText (mkFastString "print_float") Nothing True)
                    CCallConv
                    PlayRisky
                  )
                  (mkUnique 'f' 0)
                )
                [ StgLitArg $ mkMachFloat 3.14
                ] intTy
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram NCG [] $ {-unarise us-} topBinds

-- CASE: FFI double argument
sampleFFIArgDouble = do
  putStrLn "CASE: FFI with double argument"
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      topBinds  =
        [ StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgOpApp
                (StgFCallOp
                  (CCall $ CCallSpec
                    (StaticTarget NoSourceText (mkFastString "print_double") Nothing True)
                    CCallConv
                    PlayRisky
                  )
                  (mkUnique 'f' 0)
                )
                [ StgLitArg $ mkMachDouble 3.14
                ] intTy
        ]

  us <- mkSplitUniqSupply 'g'

  compileProgram NCG [] $ {-unarise us-} topBinds

-- single value vs unboxed unit ; are they interchangable?
-- CASE: construct Unit# int, unpack prim int
unboxedUnit1 = do
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      u1IntTy   = mkTupleTy Unboxed [repTy IntRep]
      idInt0    = mkIdT 100 (repTy IntRep)
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!\n1 + 2 = %d\n")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgConApp (getDataCon u1IntTy)
                  [ StgLitArg $ mkMachInt dflags 3
                  ] []
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
                    ] (repTy VoidRep)
                )
              ]
        ]
  compileProgram NCG [] topBinds

-- IMPORTANT: binders can not be unboxed tuples
-- CASE: construct prim int, unpack Unit# int into IntRep binder
unboxedUnit2 = do
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      utupTy l  = mkTupleTy Unboxed $ map repTy l
      utupDC    = getDataCon . utupTy
      idInt0    = mkIdT 100 (utupTy [IntRep])
      idInt1    = mkIdT 101 (repTy IntRep)
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!\n1 + 2 = %d\n")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (
                StgLit $ mkMachInt dflags 3
              ) idInt0 (MultiValAlt 1)
              [ (DataAlt (utupDC [IntRep]), [idInt1],
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
                    ] (repTy VoidRep)
                )
              ]
        ]
  compileProgram NCG [] topBinds

-- IMPORTANT: binders can not be void rep
-- CASE: ffi returns (##) unpack as prim VoidRep [COMPILE ERROR]
unboxedVoid1 = do
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      idStr1    = mkIdT 1 (repTy AddrRep)
      utupTy l  = mkTupleTy Unboxed $ map repTy l
      utupDC    = getDataCon . utupTy
      idVoid0   = mkIdT 100 (utupTy [])
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!")
        , StgTopStringLit idStr1 (BS8.pack "OK!")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (

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
                    ] (utupTy [])

              ) idVoid0 (PrimAlt VoidRep)
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
                    [ StgVarArg idStr1
                    ] (utupTy [])

                )
              ]
        ]
  compileProgram NCG [] topBinds

-- CASE: ffi returns prim VoidRep unpack as (##) with DataAlt + MultiValAlt 0
unboxedVoid2 = do
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      idStr1    = mkIdT 1 (repTy AddrRep)
      utupTy l  = mkTupleTy Unboxed $ map repTy l
      utupDC    = getDataCon . utupTy
      idVoid0   = mkIdT 100 (utupTy [])
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!\n")
        , StgTopStringLit idStr1 (BS8.pack "OK!\n")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (

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
                    ] (repTy VoidRep)

              ) idVoid0 (MultiValAlt 0)
              [ (DataAlt (utupDC []), [],

                  StgOpApp
                    (StgFCallOp
                      (CCall $ CCallSpec
                        (StaticTarget NoSourceText (mkFastString "printf") Nothing True)
                        CCallConv
                        PlayRisky
                      )
                      (mkUnique 'f' 0)
                    )
                    [ StgVarArg idStr1
                    ] (repTy VoidRep)

                )
              ]
        ]
  compileProgram NCG [] topBinds

-- CASE: ffi returns prim VoidRep unpack as (##) with DEFAULT + MultiValAlt 0
unboxedVoid3 = do
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      idStr1    = mkIdT 1 (repTy AddrRep)
      utupTy l  = mkTupleTy Unboxed $ map repTy l
      utupDC    = getDataCon . utupTy
      idVoid0   = mkIdT 100 (utupTy [])
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "Hello!\n")
        , StgTopStringLit idStr1 (BS8.pack "OK!\n")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (

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
                    ] (repTy VoidRep)

              ) idVoid0 (MultiValAlt 0)
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
                    [ StgVarArg idStr1
                    ] (repTy VoidRep)

                )
              ]
        ]
  compileProgram NCG [] topBinds

-- CASE: ffi void arg as (##)
unboxedVoid4 = do
  let dflags    = unsafeGlobalDynFlags
      mkIdT i t = mkVanillaGlobal (mkName i $ 'x' : show i) t
      idStr0    = mkIdT 0 (repTy AddrRep)
      utupTy l  = mkTupleTy Unboxed $ map repTy l
      utupDC    = getDataCon . utupTy
      idVoid0   = mkIdT 100 (utupTy [])
      topBinds  =
        [ StgTopStringLit idStr0 (BS8.pack "OK!\n")
        , StgTopLifted $ StgNonRec (mkIdNT 1 "main" $ repTy LiftedRep) $
            StgRhsClosure dontCareCCS {-stgSatOcc-} stgUnsatOcc [] {-SingleEntry-}Updatable [voidArgId] $
              StgCase (

                StgConApp (utupDC []) [] []

              ) idVoid0 (MultiValAlt 0)
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
                    , StgVarArg idVoid0
                    , StgVarArg idVoid0
                    , StgVarArg idVoid0
                    , StgVarArg idVoid0
                    ] (repTy VoidRep)

                )
              ]
        ]
  compileProgram NCG [] topBinds
