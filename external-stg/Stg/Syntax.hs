{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
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
  = SingleValue     PrimRep
  | UnboxedTuple    [PrimRep]
  | PolymorphicRep
  deriving (Eq, Ord, Generic, Show)

-- data con related

newtype DataConId
  = DataConId Unique
  deriving (Eq, Ord, Binary, Show)

-- raw data con
data SDataCon
  = SDataCon
  { sdcName   :: !Name
  , sdcId     :: !DataConId
  , sdcModule :: !ModuleName
  , sdcRep    :: DataConRep
  }
  deriving (Eq, Ord, Generic, Show)

data DataConRep
  = AlgDataCon      ![PrimRep]
  | UnboxedDataCon
  deriving (Eq, Ord, Generic, Show)

-- user friendly data con
data DataCon
  = DataCon
  { dcName   :: !Name
  , dcId     :: !DataConId
  , dcModule :: !ModuleName
  , dcRep    :: !DataConRep
  , dcTyCon  :: !AlgTyCon
  }
  deriving (Eq, Ord, Generic, Show)

data AlgTyCon
  = AlgTyCon
  { tcName      :: !Name
  , tcDataCons  :: ![SDataCon]
  }
  deriving (Eq, Ord, Generic, Show)

-- stg expr related

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
    }
  deriving (Eq, Ord, Generic, Show)

data Binder
  = Binder
    { binderName        :: !Name
    , binderId          :: !BinderId
    , binderType        :: !Type
    , binderTypeSig     :: !Name
    , binderModule      :: !ModuleName
    , binderIsTop       :: !Bool
    , binderIsExported  :: !Bool
    }
  deriving (Eq, Ord, Generic, Show)

binderUniqueName :: Binder -> Name
binderUniqueName Binder{..}
  | binderIsExported  = getModuleName binderModule <> BS8.pack "." <> binderName
  | otherwise         = binderName <> BS8.pack ('.' : show u)
  where BinderId u = binderId

data LitNumType
  = LitNumInteger -- ^ @Integer@ (see Note [Integer literals])
  | LitNumNatural -- ^ @Natural@ (see Note [Natural literals])
  | LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Eq, Ord, Generic, Show)

data LabelSpec
  = FunctionLabel (Maybe Int) -- only for stdcall convention
  | DataLabel
  deriving (Eq, Ord, Generic, Show)

data Lit
  = LitChar     Char
  | LitString   BS.ByteString
  | LitNullAddr
  | LitFloat    Rational
  | LitDouble   Rational
  | LitLabel    BS8.ByteString LabelSpec
  | LitNumber   LitNumType Integer
  deriving (Eq, Ord, Generic, Show)

-- | A top-level binding.
data TopBinding' idBnd idOcc dcOcc
-- See Note [CoreSyn top-level string literals]
  = StgTopLifted    (Binding' idBnd idOcc dcOcc)
  | StgTopStringLit idBnd BS.ByteString
  deriving (Eq, Ord, Generic, Show)

data Binding' idBnd idOcc dcOcc
  = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc)
  | StgRec    [(idBnd, Rhs' idBnd idOcc dcOcc)]
  deriving (Eq, Ord, Generic, Show)

data Arg' idOcc
  = StgVarArg  idOcc
  | StgLitArg  Lit
  deriving (Eq, Ord, Generic, Show)

data Expr' idBnd idOcc dcOcc
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

  | StgCase
        (Expr' idBnd idOcc dcOcc)     -- the thing to examine

        idBnd                         -- binds the result of evaluating the scrutinee

        [Alt' idBnd idOcc dcOcc]      -- The DEFAULT case is always *first*
                                      -- if it is there at all

  | StgLet
        (Binding' idBnd idOcc dcOcc)  -- right hand sides (see below)
        (Expr' idBnd idOcc dcOcc)     -- body

  | StgLetNoEscape
        (Binding' idBnd idOcc dcOcc)  -- right hand sides (see below)
        (Expr' idBnd idOcc dcOcc)     -- body
  deriving (Eq, Ord, Generic, Show)


data UpdateFlag = ReEntrant | Updatable | SingleEntry
  deriving (Eq, Ord, Generic, Show)

data Rhs' idBnd idOcc dcOcc
  = StgRhsClosure
        !UpdateFlag               -- ReEntrant | Updatable | SingleEntry
        [idBnd]                   -- arguments; if empty, then not a function;
                                  -- as above, order is important.
        (Expr' idBnd idOcc dcOcc) -- body

  | StgRhsCon
        dcOcc  -- DataCon
        [Arg' idOcc]        -- Args
  deriving (Eq, Ord, Generic, Show)

data Alt' idBnd idOcc dcOcc
  = Alt
    { altCon     :: !(AltCon' dcOcc)
    , altBinders :: [idBnd]
    , altRHS     :: Expr' idBnd idOcc dcOcc
    }
  deriving (Eq, Ord, Generic, Show)

data AltCon' dcOcc
  = AltDataCon  dcOcc
  | AltLit      Lit
  | AltDefault
  deriving (Eq, Ord, Generic, Show)

data Safety = PlaySafe | PlayInterruptible | PlayRisky
  deriving (Eq, Ord, Generic, Show)

data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
  deriving (Eq, Ord, Generic, Show)

data CCallTarget
  = StaticTarget BS8.ByteString
  | DynamicTarget
  deriving (Eq, Ord, Generic, Show)

data ForeignCall
  = ForeignCall
  { foreignCTarget  :: CCallTarget
  , foreignCConv    :: CCallConv
  , foreignCSafety  :: Safety
  }
  deriving (Eq, Ord, Generic, Show)

data PrimCall = PrimCall -- BS8.ByteString BS8.ByteString
  deriving (Eq, Ord, Generic, Show)

data StgOp
  = StgPrimOp     Name
  | StgPrimCallOp PrimCall
  | StgFCallOp    ForeignCall
  deriving (Eq, Ord, Generic, Show)

data Module' idBnd idOcc dcOcc
  = Module
  { modulePhase           :: !BS8.ByteString
  , moduleName            :: !ModuleName
  , moduleDependency      :: ![ModuleName]
  , moduleExternalTopIds  :: ![(ModuleName, [idBnd])]
  , moduleAlgTyCons       :: ![(ModuleName, [AlgTyCon])]
  , moduleExported        :: ![(ModuleName, [BinderId])]
  , moduleTopBindings     :: ![TopBinding' idBnd idOcc dcOcc]
  , moduleCoreSrc         :: !BS8.ByteString
  , modulePrepCoreSrc     :: !BS8.ByteString
  }
  deriving (Eq, Ord, Generic, Show)

-- convenience layers: raw and user friendly

-- raw - as it is serialized
type STopBinding = TopBinding'  SBinder BinderId DataConId
type SBinding    = Binding'     SBinder BinderId DataConId
type SExpr       = Expr'        SBinder BinderId DataConId
type SRhs        = Rhs'         SBinder BinderId DataConId
type SAlt        = Alt'         SBinder BinderId DataConId
type SModule     = Module'      SBinder BinderId DataConId
type SAltCon     = AltCon'      DataConId
type SArg        = Arg'         BinderId

-- user friendly - rich information
type TopBinding = TopBinding'  Binder Binder DataCon
type Binding    = Binding'     Binder Binder DataCon
type Expr       = Expr'        Binder Binder DataCon
type Rhs        = Rhs'         Binder Binder DataCon
type Alt        = Alt'         Binder Binder DataCon
type Module     = Module'      Binder Binder DataCon
type AltCon     = AltCon'      DataCon
type Arg        = Arg'         Binder

instance Binary Unique
instance Binary PrimElemRep
instance Binary PrimRep
instance Binary Type
instance Binary Binder
instance Binary SBinder
instance Binary LitNumType
instance Binary LabelSpec
instance Binary Lit
instance Binary CCallTarget
instance Binary CCallConv
instance Binary Safety
instance Binary ForeignCall
instance Binary PrimCall
instance Binary UpdateFlag
instance Binary StgOp
instance Binary DataConRep
instance Binary SDataCon
instance Binary AlgTyCon
instance (Binary dcOcc) => Binary (AltCon' dcOcc)
instance (Binary idOcc) => Binary (Arg' idOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc) => Binary (TopBinding' idBnd idOcc dcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc) => Binary (Binding' idBnd idOcc dcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc) => Binary (Expr' idBnd idOcc dcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc) => Binary (Rhs' idBnd idOcc dcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc) => Binary (Alt' idBnd idOcc dcOcc)
instance (Binary idBnd, Binary idOcc, Binary dcOcc) => Binary (Module' idBnd idOcc dcOcc)
