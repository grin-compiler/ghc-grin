{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module GhcDump_StgAst where

import GHC.Generics

import qualified Data.ByteString as BS
import Data.Binary

import GhcDump_Ast (Lit, AltCon, Unique, T_Text, ModuleName, Binder, SBinder, BinderId)

data DataCon
  = DataCon T_Text
  deriving (Eq, Ord, Generic, Show)

data ForeignCall
  = ForeignCall -- TODO
  deriving (Eq, Ord, Generic, Show)

data PrimOp
  = PrimOp T_Text
  deriving (Eq, Ord, Generic, Show)

data PrimCall = PrimCall -- T_Text T_Text
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
  | StgConApp   DataCon
                [Arg' occ] -- Saturated

  | StgOpApp    StgOp           -- Primitive op or foreign call
                [Arg' occ] -- Saturated.

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
        BinderInfo           -- Info about how this binder is used (see below)
        [occ]                   -- non-global free vars; a list, rather than
                                -- a set, because order is important
        !UpdateFlag             -- ReEntrant | Updatable | SingleEntry
        [bndr]                  -- arguments; if empty, then not a function;
                                -- as above, order is important.
        (Expr' bndr occ)   -- body

  | StgRhsCon
        DataCon          -- Constructor. Never an unboxed tuple or sum, as those
                         -- are not allocated.
        [Arg' occ]  -- Args
  deriving (Eq, Ord, Generic, Show)

data BinderInfo
  = NoStgBinderInfo
  | SatCallsOnly        -- All occurrences are *saturated* *function* calls
                        -- This means we don't need to build an info table and
                        -- slow entry code for the thing
                        -- Thunks never get this value
  deriving (Eq, Ord, Generic, Show)

type SAlt = Alt' SBinder BinderId
type Alt  = Alt' Binder Binder

data Alt' bndr occ
  = Alt
    { altCon     :: !AltCon
    , altBinders :: [bndr]
    , altRHS     :: Expr' bndr occ
    }
  deriving (Eq, Ord, Generic, Show)


data UpdateFlag = ReEntrant | Updatable | SingleEntry
  deriving (Eq, Ord, Generic, Show)

data StgOp
  = StgPrimOp  PrimOp

  | StgPrimCallOp PrimCall

  | StgFCallOp ForeignCall Unique
        -- The Unique is occasionally needed by the C pretty-printer
        -- (which lacks a unique supply), notably when generating a
        -- typedef for foreign-export-dynamic
  deriving (Eq, Ord, Generic, Show)

type Module  = Module' Binder Binder
type SModule = Module' SBinder BinderId

data Module' bndr occ
  = Module
    { moduleName        :: ModuleName
    , modulePhase       :: T_Text
    , moduleTopBindings :: [TopBinding' bndr occ]
    }
  deriving (Eq, Ord, Generic, Show)

instance Binary DataCon
instance Binary ForeignCall
instance Binary PrimOp
instance Binary PrimCall
instance Binary BinderInfo
instance Binary UpdateFlag
instance Binary StgOp
instance (Binary occ) => Binary (Arg' occ)
instance (Binary bndr, Binary occ) => Binary (TopBinding' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Binding' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Expr' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Rhs' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Alt' bndr occ)
instance (Binary bndr, Binary occ) => Binary (Module' bndr occ)
