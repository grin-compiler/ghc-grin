{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module GhcDump_StgAst where

import GhcPrelude

import GHC.Generics

import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Binary

type T_Text = BS8.ByteString

data Unique
  = Unique !Char !Int
  deriving (Eq, Ord, Generic)

instance Show Unique where
 show (Unique c n) = c : show n

data PrimRep
  = VoidRep
  | LiftedRep
  | UnliftedRep   -- ^ Unlifted pointer
  | IntRep        -- ^ Signed, word-sized value
  | WordRep       -- ^ Unsigned, word-sized value
  | Int64Rep      -- ^ Signed, 64 bit value (with 32-bit words only)
  | Word64Rep     -- ^ Unsigned, 64 bit value (with 32-bit words only)
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

newtype TyConId
  = TyConId Unique
  deriving (Eq, Ord, Binary, Show)

data TyCon' occ
  = TyCon
  { tcName      :: T_Text
  , tcId        :: TyConId
  , tcDataCons  :: [occ]
  }
  deriving (Eq, Ord, Generic, Show)

type STyCon = TyCon' BinderId
type TyCon  = TyCon' Binder

data TypeInfo
  = TypeInfo
  { tRep   :: !(Maybe [PrimRep])
  , tTyCon :: !(Maybe TyConId)
  , tType  :: T_Text
  }
  deriving (Eq, Ord, Generic, Show)

newtype ModuleName
  = ModuleName { getModuleName :: T_Text }
  deriving (Eq, Ord, Binary, Show)

newtype BinderId
  = BinderId Unique
  deriving (Eq, Ord, Binary, Show)

data SBinder
  = SBinder
    { sbinderName :: !T_Text
    , sbinderId   :: !BinderId
    , sbinderType :: !TypeInfo
    }
  deriving (Eq, Ord, Generic, Show)

data Binder
  = Binder
    { binderName        :: !T_Text
    , binderId          :: !BinderId
    , binderType        :: !TypeInfo
    , binderModule      :: !ModuleName
    , binderIsTop       :: !Bool
    , binderIsExported  :: !Bool
    }
  deriving (Eq, Ord, Generic, Show)

binderUniqueName :: Binder -> T_Text
binderUniqueName Binder{..}
  | binderIsExported = getModuleName binderModule <> BS8.pack "." <> binderName
  | otherwise   = binderName <> BS8.pack ('.' : show u)
  where BinderId u = binderId

data LitNumType
  = LitNumInteger -- ^ @Integer@ (see Note [Integer literals])
  | LitNumNatural -- ^ @Natural@ (see Note [Natural literals])
  | LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Eq, Ord, Generic, Show)

data Lit
  = MachChar      Char
  | MachStr       BS.ByteString
  | MachNullAddr
  | MachFloat     Rational
  | MachDouble    Rational
  | MachLabel     T_Text
  | LitNumber     LitNumType Integer
  deriving (Eq, Ord, Generic, Show)


type STopBinding = TopBinding' SBinder BinderId
type TopBinding  = TopBinding' Binder Binder

-- | A top-level binding.
data TopBinding' bndr occ
-- See Note [CoreSyn top-level string literals]
  = StgTopLifted (Binding' bndr occ)
  | StgTopStringLit bndr BS.ByteString
  deriving (Eq, Ord, Generic, Show)

type SBinding = Binding' SBinder BinderId
type Binding  = Binding' Binder Binder

data Binding' bndr occ
  = StgNonRec bndr (Rhs' bndr occ)
  | StgRec    [(bndr, Rhs' bndr occ)]
  deriving (Eq, Ord, Generic, Show)

type SArg = Arg' BinderId
type Arg  = Arg' Binder

data Arg' occ
  = StgVarArg  occ
  | StgLitArg  Lit
  deriving (Eq, Ord, Generic, Show)

type SExpr = Expr' SBinder BinderId
type Expr  = Expr' Binder Binder

data Expr' bndr occ
  = StgApp
        occ             -- function
        [Arg' occ] -- arguments; may be empty

  | StgLit      Lit

        -- StgConApp is vital for returning unboxed tuples or sums
        -- which can't be let-bound first
  | StgConApp   occ        -- DataCon
                [Arg' occ] -- Saturated
                [TypeInfo] -- types

  | StgOpApp    StgOp      -- Primitive op or foreign call
                [Arg' occ] -- Saturated.
                TypeInfo   -- result type

  | StgLam
        [bndr]
        (Expr' bndr occ)   -- Body of lambda

  | StgCase
        (Expr' bndr occ)
                    -- the thing to examine

        bndr        -- binds the result of evaluating the scrutinee

        [Alt' bndr occ]
                    -- The DEFAULT case is always *first*
                    -- if it is there at all

  | StgLet
        (Binding' bndr occ)    -- right hand sides (see below)
        (Expr' bndr occ)       -- body

  | StgLetNoEscape
        (Binding' bndr occ)    -- right hand sides (see below)
        (Expr' bndr occ)       -- body
  deriving (Eq, Ord, Generic, Show)

type SRhs = Rhs' SBinder BinderId
type Rhs  = Rhs' Binder Binder

data Rhs' bndr occ
  = StgRhsClosure
        [occ]                   -- non-global free vars; a list, rather than
                                -- a set, because order is important
        !UpdateFlag             -- ReEntrant | Updatable | SingleEntry
        [bndr]                  -- arguments; if empty, then not a function;
                                -- as above, order is important.
        (Expr' bndr occ)   -- body

  | StgRhsCon
        occ         -- DataCon
        [Arg' occ]  -- Args
  deriving (Eq, Ord, Generic, Show)

data UpdateFlag = ReEntrant | Updatable | SingleEntry
  deriving (Eq, Ord, Generic, Show)

type SAlt = Alt' SBinder BinderId
type Alt  = Alt' Binder Binder

data Alt' bndr occ
  = Alt
    { altCon     :: !(AltCon' occ)
    , altBinders :: [bndr]
    , altRHS     :: Expr' bndr occ
    }
  deriving (Eq, Ord, Generic, Show)

type SAltCon = AltCon' BinderId
type AltCon = AltCon' Binder

data AltCon' occ
  = AltDataCon  occ -- DataCon
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

data PrimCall = PrimCall -- T_Text T_Text
  deriving (Eq, Ord, Generic, Show)

data StgOp
  = StgPrimOp     T_Text
  | StgPrimCallOp PrimCall
  | StgFCallOp    ForeignCall
  deriving (Eq, Ord, Generic, Show)

type Module  = Module' Binder Binder
type SModule = Module' SBinder BinderId

data Module' bndr occ
  = Module
    { modulePhase       :: T_Text
    , moduleName        :: ModuleName
    , moduleDependency  :: [ModuleName]
    , moduleExternals   :: [(ModuleName, [bndr])]
    , moduleDataCons    :: [(ModuleName, [bndr])]
    , moduleTyCons      :: [(ModuleName, [TyCon' occ])]
    , moduleExported    :: [(ModuleName, [BinderId])]
    , moduleTopBindings :: [TopBinding' bndr occ]
    }
  deriving (Eq, Ord, Generic, Show)

instance Binary Unique
instance Binary PrimElemRep
instance Binary PrimRep
instance Binary TypeInfo
instance Binary Binder
instance Binary SBinder
instance Binary LitNumType
instance Binary Lit
instance Binary CCallTarget
instance Binary CCallConv
instance Binary Safety
instance Binary ForeignCall
instance Binary PrimCall
instance Binary UpdateFlag
instance Binary StgOp
instance (Binary occ) => Binary (TyCon' occ)
instance (Binary occ) => Binary (AltCon' occ)
instance (Binary occ) => Binary (Arg' occ)
instance (Binary bndr, Binary occ) => Binary (TopBinding' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Binding' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Expr' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Rhs' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Alt' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Module' bndr occ)
