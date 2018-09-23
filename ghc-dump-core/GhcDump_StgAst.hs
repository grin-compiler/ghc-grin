{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module GhcDump_StgAst where

import GHC.Generics

import qualified Data.ByteString as BS
import Data.Binary

import GhcDump_Ast (Lit, AltCon, Unique, T_Text, ModuleName, SBinder, BinderId)

data DataCon
  = DataCon
  deriving (Eq, Ord, Generic, Show)

data ForeignCall  -- dummmy
  = ForeignCall
  deriving (Eq, Ord, Generic, Show)

data PrimOp       -- TODO: long enum
  = PrimOp
  deriving (Eq, Ord, Generic, Show)

data PrimCall = PrimCall T_Text T_Text
  deriving (Eq, Ord, Generic, Show)

-- | A top-level binding.
data GenStgTopBinding bndr occ
-- See Note [CoreSyn top-level string literals]
  = StgTopLifted (GenStgBinding bndr occ)
  | StgTopStringLit bndr BS.ByteString
  deriving (Eq, Ord, Generic, Show)

data GenStgBinding bndr occ
  = StgNonRec bndr (GenStgRhs bndr occ)
  | StgRec    [(bndr, GenStgRhs bndr occ)]
  deriving (Eq, Ord, Generic, Show)

data GenStgArg occ
  = StgVarArg  occ
  | StgLitArg  Lit
  deriving (Eq, Ord, Generic, Show)

data GenStgExpr bndr occ
  = StgApp
        occ             -- function
        [GenStgArg occ] -- arguments; may be empty

  | StgLit      Lit

        -- StgConApp is vital for returning unboxed tuples or sums
        -- which can't be let-bound first
  | StgConApp   DataCon
                [GenStgArg occ] -- Saturated

  | StgOpApp    StgOp           -- Primitive op or foreign call
                [GenStgArg occ] -- Saturated.

  | StgLam
        [bndr]
        (GenStgExpr bndr occ)   -- Body of lambda

  | StgCase
        (GenStgExpr bndr occ)
                    -- the thing to examine

        bndr        -- binds the result of evaluating the scrutinee

        [GenStgAlt bndr occ]
                    -- The DEFAULT case is always *first*
                    -- if it is there at all

  | StgLet
        (GenStgBinding bndr occ)    -- right hand sides (see below)
        (GenStgExpr bndr occ)       -- body

  | StgLetNoEscape
        (GenStgBinding bndr occ)    -- right hand sides (see below)
        (GenStgExpr bndr occ)       -- body
  deriving (Eq, Ord, Generic, Show)

data GenStgRhs bndr occ
  = StgRhsClosure
        StgBinderInfo           -- Info about how this binder is used (see below)
        [occ]                   -- non-global free vars; a list, rather than
                                -- a set, because order is important
        !UpdateFlag             -- ReEntrant | Updatable | SingleEntry
        [bndr]                  -- arguments; if empty, then not a function;
                                -- as above, order is important.
        (GenStgExpr bndr occ)   -- body

  | StgRhsCon
        DataCon          -- Constructor. Never an unboxed tuple or sum, as those
                         -- are not allocated.
        [GenStgArg occ]  -- Args
  deriving (Eq, Ord, Generic, Show)

data StgBinderInfo
  = NoStgBinderInfo
  | SatCallsOnly        -- All occurrences are *saturated* *function* calls
                        -- This means we don't need to build an info table and
                        -- slow entry code for the thing
                        -- Thunks never get this value
  deriving (Eq, Ord, Generic, Show)

data GenStgAlt bndr var
  = StgAlt
    { altCon     :: !AltCon
    , altBinders :: [bndr]
    , altRHS     :: GenStgExpr bndr var
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

data Module' bndr var
  = Module
    { moduleName        :: ModuleName
    , modulePhase       :: T_Text
    , moduleTopBindings :: [GenStgTopBinding bndr var]
    }
  deriving (Eq, Ord, Generic, Show)

type SModule = Module' SBinder BinderId

instance Binary DataCon
instance Binary ForeignCall
instance Binary PrimOp
instance Binary PrimCall
instance Binary StgBinderInfo
instance Binary UpdateFlag
instance Binary StgOp
instance (Binary var) => Binary (GenStgArg var)
instance (Binary bndr, Binary var) => Binary (GenStgTopBinding bndr var)
instance (Binary bndr, Binary var) => Binary (GenStgBinding bndr var)
instance (Binary bndr, Binary var) => Binary (GenStgExpr bndr var)
instance (Binary bndr, Binary var) => Binary (GenStgRhs bndr var)
instance (Binary bndr, Binary var) => Binary (GenStgAlt bndr var)
instance (Binary bndr, Binary var) => Binary (Module' bndr var)
