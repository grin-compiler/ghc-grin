{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Stg.Syntax where

import GhcPrelude

import GHC.Generics

import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Binary

-- data types

type Name = BS8.ByteString

data Unique
  = Unique !Char !Int
  deriving (Eq, Ord, Generic)

instance Show Unique where
 show (Unique c n) = c : show n

-- type related

data PrimRep
  = VoidRep
  | LiftedRep
  | UnliftedRep   -- ^ Unlifted pointer
  | Int8Rep       -- ^ Signed, 8-bit value
  | Int16Rep      -- ^ Signed, 16-bit value
  | Int32Rep      -- ^ Signed, 32-bit value
  | Int64Rep      -- ^ Signed, 64 bit value (with 32-bit words only)
  | IntRep        -- ^ Signed, word-sized value
  | Word8Rep      -- ^ Unsigned, 8 bit value
  | Word16Rep     -- ^ Unsigned, 16 bit value
  | Word32Rep     -- ^ Unsigned, 32 bit value
  | Word64Rep     -- ^ Unsigned, 64 bit value (with 32-bit words only)
  | WordRep       -- ^ Unsigned, word-sized value
  | AddrRep       -- ^ A pointer, but /not/ to a Haskell value (use '(Un)liftedRep')
  | FloatRep
  | DoubleRep
  | VecRep Int PrimElemRep  -- ^ A vector
  deriving (Eq, Ord, Generic, Show)

data PrimElemRep
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep
  deriving (Eq, Ord, Generic, Show)


{-
  Q: do we want to keep haskell types OR would representation type system be enough?
  A: keep only those information that is relevant for the codegen

  HINT: extrenal STG and lambda IR should be identical
-}

data Type
  = SingleValue     !PrimRep
  | UnboxedTuple    ![PrimRep]
  | PolymorphicRep
  deriving (Eq, Ord, Generic, Show)

-- data con related

newtype TyConId
  = TyConId Unique
  deriving (Eq, Ord, Binary, Show)

newtype DataConId
  = DataConId Unique
  deriving (Eq, Ord, Binary, Show)

-- raw data con
data DataConRep
  = AlgDataCon      ![PrimRep]
  | UnboxedTupleCon !Int
  deriving (Eq, Ord, Generic, Show)

data SDataCon
  = SDataCon
  { sdcName   :: !Name
  , sdcId     :: !DataConId
  , sdcRep    :: !DataConRep
  , sdcWorker :: !SBinder
  }
  deriving (Eq, Ord, Generic, Show)

data STyCon
  = STyCon
  { stcName     :: !Name
  , stcId       :: !TyConId
  , stcDataCons :: ![SDataCon]
  }
  deriving (Eq, Ord, Generic, Show)

-- user friendly data con
data DataCon
  = DataCon
  { dcName   :: !Name
  , dcId     :: !DataConId
  , dcUnitId :: !UnitId
  , dcModule :: !ModuleName
  , dcRep    :: !DataConRep
  , dcTyCon  :: !TyCon
  , dcWorker :: !Binder
  }
  deriving (Eq, Ord, Generic, Show)

data TyCon
  = TyCon
  { tcName      :: !Name
  , tcId        :: !TyConId
  , tcUnitId    :: !UnitId
  , tcModule    :: !ModuleName
  , tcDataCons  :: ![DataCon]
  }
  deriving (Eq, Ord, Generic, Show)

-- id info

data IdDetails
  = VanillaId
  | FExportedId
  | RecSelId
  | DataConWorkId DataConId
  | DataConWrapId DataConId
  | ClassOpId
  | PrimOpId
  | FCallId
  | TickBoxOpId
  | DFunId
  | CoVarId
  | JoinId        Int
  deriving (Eq, Ord, Generic, Show)

-- stg expr related

newtype UnitId
  = UnitId { getUnitId :: Name }
  deriving (Eq, Ord, Binary, Show)

newtype ModuleName
  = ModuleName { getModuleName :: Name }
  deriving (Eq, Ord, Binary, Show)

newtype BinderId
  = BinderId Unique
  deriving (Eq, Ord, Binary, Show)

data SBinder
  = SBinder
    { sbinderName     :: !Name
    , sbinderId       :: !BinderId
    , sbinderType     :: !Type
    , sbinderTypeSig  :: !Name
    , sbinderScope    :: !Scope
    , sbinderDetails  :: !IdDetails
    }
  deriving (Eq, Ord, Generic, Show)

data Binder
  = Binder
    { binderName      :: !Name
    , binderId        :: !BinderId
    , binderType      :: !Type
    , binderTypeSig   :: !Name
    , binderScope     :: !Scope
    , binderDetails   :: !IdDetails
    , binderUnitId    :: !UnitId
    , binderModule    :: !ModuleName
    }
  deriving (Eq, Ord, Generic, Show)

data Scope
  = LocalScope      -- ^ visible for expression body
  | GlobalScope     -- ^ visible for a single haskell module
  | HaskellExported -- ^ visible for every haskell module
  | ForeignExported -- ^ visible for foreign libraries
  deriving (Eq, Ord, Generic, Show)

binderUniqueName :: Binder -> Name
binderUniqueName Binder{..} = case binderScope of
  LocalScope  -> binderName <> BS8.pack ('.' : show u)
  GlobalScope -> getUnitId binderUnitId <> "_" <> getModuleName binderModule <> "." <> binderName <> BS8.pack ('.' : show u)
  _           -> getUnitId binderUnitId <> "_" <> getModuleName binderModule <> "." <> binderName
  where
    BinderId u = binderId

data LitNumType
  = LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Eq, Ord, Generic, Show)

data LabelSpec
  = FunctionLabel !(Maybe Int) -- only for stdcall convention
  | DataLabel
  deriving (Eq, Ord, Generic, Show)

data Lit
  = LitChar     !Char
  | LitString   !BS.ByteString
  | LitNullAddr
  | LitFloat    !Rational
  | LitDouble   !Rational
  | LitLabel    !BS8.ByteString LabelSpec
  | LitNumber   !LitNumType !Integer
  deriving (Eq, Ord, Generic, Show)

-- | A top-level binding.
data TopBinding' idBnd idOcc dcOcc tcOcc
-- See Note [CoreSyn top-level string literals]
  = StgTopLifted    (Binding' idBnd idOcc dcOcc tcOcc)
  | StgTopStringLit idBnd BS.ByteString
  deriving (Eq, Ord, Generic, Show)

data Binding' idBnd idOcc dcOcc tcOcc
  = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
  | StgRec    [(idBnd, Rhs' idBnd idOcc dcOcc tcOcc)]
  deriving (Eq, Ord, Generic, Show)

data Arg' idOcc
  = StgVarArg  idOcc
  | StgLitArg  !Lit
  deriving (Eq, Ord, Generic, Show)

data Expr' idBnd idOcc dcOcc tcOcc
  = StgApp
        idOcc         -- function
        [Arg' idOcc]  -- arguments; may be empty
        Type          -- result type
        (Name,Name,Name)  -- fun core type pp, result core type pp, StgApp oigin (Var/Coercion/App)

  | StgLit      Lit

        -- StgConApp is vital for returning unboxed tuples or sums
        -- which can't be let-bound first
  | StgConApp   dcOcc         -- DataCon
                [Arg' idOcc]  -- Saturated
                [Type]        -- types

  | StgOpApp    StgOp         -- Primitive op or foreign call
                [Arg' idOcc]  -- Saturated.
                Type          -- result type
                (Maybe tcOcc) -- result type name (required for tagToEnum wrapper generator)

  | StgCase
        (Expr' idBnd idOcc dcOcc tcOcc)     -- the thing to examine

        idBnd                               -- binds the result of evaluating the scrutinee
        (AltType' tcOcc)
        [Alt' idBnd idOcc dcOcc tcOcc]      -- The DEFAULT case is always *first*
                                            -- if it is there at all

  | StgLet
        (Binding' idBnd idOcc dcOcc tcOcc)  -- right hand sides (see below)
        (Expr' idBnd idOcc dcOcc tcOcc)     -- body

  | StgLetNoEscape
        (Binding' idBnd idOcc dcOcc tcOcc)  -- right hand sides (see below)
        (Expr' idBnd idOcc dcOcc tcOcc)     -- body
  deriving (Eq, Ord, Generic, Show)

data AltType' tcOcc
  = PolyAlt
  | MultiValAlt !Int
  | PrimAlt     !PrimRep
  | AlgAlt      tcOcc
  deriving (Eq, Ord, Generic, Show)

data UpdateFlag = ReEntrant | Updatable | SingleEntry
  deriving (Eq, Ord, Generic, Show)

data Rhs' idBnd idOcc dcOcc tcOcc
  = StgRhsClosure
        !UpdateFlag               -- ReEntrant | Updatable | SingleEntry
        [idBnd]                   -- arguments; if empty, then not a function;
                                  -- as above, order is important.
        (Expr' idBnd idOcc dcOcc tcOcc) -- body

  | StgRhsCon
        dcOcc  -- DataCon
        [Arg' idOcc]        -- Args
  deriving (Eq, Ord, Generic, Show)

data Alt' idBnd idOcc dcOcc tcOcc
  = Alt
    { altCon     :: !(AltCon' dcOcc)
    , altBinders :: [idBnd]
    , altRHS     :: Expr' idBnd idOcc dcOcc tcOcc
    }
  deriving (Eq, Ord, Generic, Show)

data AltCon' dcOcc
  = AltDataCon  dcOcc
  | AltLit      !Lit
  | AltDefault
  deriving (Eq, Ord, Generic, Show)

data Safety = PlaySafe | PlayInterruptible | PlayRisky
  deriving (Eq, Ord, Generic, Show)

data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
  deriving (Eq, Ord, Generic, Show)

data SourceText
  = SourceText    !BS8.ByteString
  | NoSourceText
  deriving (Eq, Ord, Generic, Show)

data CCallTarget
  = StaticTarget !SourceText !BS8.ByteString !(Maybe UnitId) !Bool
  | DynamicTarget
  deriving (Eq, Ord, Generic, Show)

data ForeignCall
  = ForeignCall
  { foreignCTarget  :: !CCallTarget
  , foreignCConv    :: !CCallConv
  , foreignCSafety  :: !Safety
  }
  deriving (Eq, Ord, Generic, Show)

data PrimCall = PrimCall !BS8.ByteString !UnitId
  deriving (Eq, Ord, Generic, Show)

data StgOp
  = StgPrimOp     !Name
  | StgPrimCallOp !PrimCall
  | StgFCallOp    !ForeignCall
  deriving (Eq, Ord, Generic, Show)

-- foreign export stubs

data ForeignStubs
  = NoStubs
  | ForeignStubs
    { fsCHeader :: !BS8.ByteString
    , fsCSource :: !BS8.ByteString
    }
  deriving (Eq, Ord, Generic, Show)

data ForeignSrcLang
  = LangC      -- ^ C
  | LangCxx    -- ^ C++
  | LangObjc   -- ^ Objective C
  | LangObjcxx -- ^ Objective C++
  | LangAsm    -- ^ Assembly language (.s)
  | RawObject  -- ^ Object (.o)
  deriving (Eq, Ord, Generic, Show)

-- the whole module

data Module' idBnd idOcc dcOcc tcBnd tcOcc
  = Module
  { modulePhase               :: !BS8.ByteString
  , moduleUnitId              :: !UnitId
  , moduleName                :: !ModuleName
  , moduleForeignStubs        :: !ForeignStubs
  , moduleHasForeignExported  :: !Bool
  , moduleDependency          :: ![(UnitId, [ModuleName])]
  , moduleExternalTopIds      :: ![(UnitId, [(ModuleName, [idBnd])])]
  , moduleTyCons              :: ![(UnitId, [(ModuleName, [tcBnd])])]
  , moduleTopBindings         :: ![TopBinding' idBnd idOcc dcOcc tcOcc]
  , moduleForeignFiles        :: ![(ForeignSrcLang, FilePath)]
  , moduleCoreSrc             :: !BS8.ByteString
  , modulePrepCoreSrc         :: !BS8.ByteString
  , moduleStgSrc              :: !BS8.ByteString
  }
  deriving (Eq, Ord, Generic, Show)

-- convenience layers: raw and user friendly

-- raw - as it is serialized
type SModule     = Module'      SBinder BinderId DataConId STyCon  TyConId
type STopBinding = TopBinding'  SBinder BinderId DataConId TyConId
type SBinding    = Binding'     SBinder BinderId DataConId TyConId
type SExpr       = Expr'        SBinder BinderId DataConId TyConId
type SRhs        = Rhs'         SBinder BinderId DataConId TyConId
type SAlt        = Alt'         SBinder BinderId DataConId TyConId
type SAltCon     = AltCon'      DataConId
type SAltType    = AltType'     TyConId
type SArg        = Arg'         BinderId

-- user friendly - rich information
type Module     = Module'      Binder Binder DataCon TyCon TyCon
type TopBinding = TopBinding'  Binder Binder DataCon TyCon
type Binding    = Binding'     Binder Binder DataCon TyCon
type Expr       = Expr'        Binder Binder DataCon TyCon
type Rhs        = Rhs'         Binder Binder DataCon TyCon
type Alt        = Alt'         Binder Binder DataCon TyCon
type AltCon     = AltCon'      DataCon
type AltType    = AltType'     TyCon
type Arg        = Arg'         Binder

instance Binary Unique
instance Binary PrimElemRep
instance Binary PrimRep
instance Binary Type
instance Binary IdDetails
instance Binary Scope
instance Binary Binder
instance Binary SBinder
instance Binary LitNumType
instance Binary LabelSpec
instance Binary Lit
instance Binary SourceText
instance Binary CCallTarget
instance Binary CCallConv
instance Binary Safety
instance Binary ForeignCall
instance Binary PrimCall
instance Binary UpdateFlag
instance Binary StgOp
instance Binary DataConRep
instance Binary SDataCon
instance Binary STyCon
instance Binary ForeignStubs
instance Binary ForeignSrcLang
instance (Binary tcOcc) => Binary (AltType' tcOcc)
instance (Binary dcOcc) => Binary (AltCon' dcOcc)
instance (Binary idOcc) => Binary (Arg' idOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (TopBinding' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Binding' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Rhs' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Alt' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc) => Binary (Expr' idBnd idOcc dcOcc tcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc, Binary tcOcc, Binary tcBnd) => Binary (Module' idBnd idOcc dcOcc tcOcc tcBnd)
