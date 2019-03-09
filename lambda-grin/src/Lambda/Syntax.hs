{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, DeriveDataTypeable #-}
module Lambda.Syntax
  ( module Lambda.Syntax
  , Grin.packName
  , Grin.unpackName
  , Grin.showTS
  ) where

import GHC.Generics
import Data.Data
import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Data.Functor.Foldable as Foldable
import Data.Functor.Foldable.TH
import Data.Binary
import Data.Text (Text)
import qualified Grin.Grin as Grin

type Name = Grin.Name

data Ty
  = TyCon     Name [Ty]
  | TyVar     Name
  | TySimple  SimpleType
  deriving (Generic, Data, Eq, Ord, Show)

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Double  -- TODO: missing from GRIN
  | T_Bool
  | T_Unit
  | T_String
  | T_Char
  | T_Addr    -- TODO: missing from GRIN
  deriving (Generic, Data, Eq, Ord, Show)

data External
  = External
  { eName       :: Name
  , eRetType    :: Ty
  , eArgsType   :: [Ty]
  , eEffectful  :: Bool
  }
  deriving (Generic, Data, Eq, Ord, Show)

type Atom = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [External] [Def]
  -- Binding
  | Def         Name [Name] Exp
  -- Exp
  | App         Name [Atom]
  | Case        Atom [Alt]
  | Let         [(Name, Exp)] Exp -- lazy let
  | LetRec      [(Name, Exp)] Exp -- lazy let with mutually recursive bindings
  | LetS        [(Name, Exp)] Exp -- strict let
  | Con         Name [Atom]
  -- Atom
--  | Var         Name
  | Var         Bool Name -- is pointer
  | Lit         Lit
  -- Alt
  | Alt         Pat Exp
  -- Extra
  | AppExp      Exp [Exp]         -- convenient for nested expressions i.e. lambdas
  | Lam         [Name] Exp
  deriving (Generic, Data, Eq, Ord, Show)

data Lit
  = LInt64      Int64
  | LWord64     Word64
  | LFloat      Rational
  | LDouble     Rational    -- TODO: missing from GRIN
  | LBool       Bool
  | LChar       Char
  | LString     ByteString
  | LLabelAddr  ByteString  -- TODO: missing from GRIN
  | LNullAddr               -- TODO: missing from GRIN
  -- special
  | LError  !Text  -- marks an error ; dead code elimination may elminate
--  | LDummy  Text  -- should be ignored
  deriving (Generic, Data, Eq, Ord, Show)

data Pat
  = NodePat Name [Name]
  | LitPat  Lit
  | DefaultPat
  deriving (Generic, Data, Eq, Show, Ord)

-- TODO: do we need lambda?

makeBaseFunctor ''Exp

instance Binary Exp
instance Binary Lit
instance Binary Pat
instance Binary Ty
instance Binary SimpleType
instance Binary External
