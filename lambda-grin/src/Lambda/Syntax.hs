{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, DeriveDataTypeable #-}
module Lambda.Syntax
  ( module Lambda.Syntax
  , Grin.packName
  , Grin.unpackName
  , Grin.showTS
  , Ty(..)
  , SimpleType(..)
  , ExternalKind(..)
  , External(..)
  , Lit(..)
  , Pat(..)
  , StaticData(..)
  , StaticValue(..)
  , PrimRep(..)
  , RepType(..)
  , ConSpec(..)
  , ConGroup(..)
  ) where

import GHC.Generics
import Data.Data
import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Data.Functor.Foldable as Foldable
import Data.Functor.Foldable.TH
import Data.Store
import Data.Store.TH
import Data.Text (Text)
import qualified Grin.Grin as Grin

type Name = Grin.Name
type ConName = Name

data Ty
  = TyCon     Name ConName [Ty]
  | TyVar     Name
  | TySimple  Name SimpleType
  | TyFun     Name Ty [Ty] -- ret ; args
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
  | T_Token   ByteString
  deriving (Generic, Data, Eq, Ord, Show)

data ExternalKind
  = PrimOp -- ^ Implemented in the internal code generator
  | FFI    -- ^ Implemented in C and linked during the linker phase
  deriving (Generic, Data, Eq, Ord, Show)

data External
  = External
  { eName       :: Name
  , eRetType    :: Ty
  , eArgsType   :: [Ty]
  , eEffectful  :: Bool
  , eKind       :: ExternalKind
  }
  deriving (Generic, Data, Eq, Ord, Show)

data StaticData
  = StaticData
  { sName   :: Name
  , sValue  :: StaticValue
  }
  deriving (Generic, Data, Eq, Ord, Show)

data StaticValue
  = StaticString ByteString
  deriving (Generic, Data, Eq, Ord, Show)

-- representation types (for codegen)
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
  | PolymorphicRep
  deriving (Generic, Data, Eq, Show, Ord)

-- data constructor declaration
data ConSpec
  = ConSpec
  { csName    :: Name
  , csArgsRep :: [PrimRep]
  }
  deriving (Generic, Data, Eq, Show, Ord)

data ConGroup
  = ConGroup
  { cgName  :: Name
  , cgCons  :: [ConSpec]
  }
  deriving (Generic, Data, Eq, Show, Ord)

-- lambda ast

type Alt        = Exp
type Def        = Exp
type Program    = Exp
type BindChain  = Exp
type SimpleExp  = Exp

data Exp
  = Program     [External] [ConGroup] [StaticData] [Def]
  -- Binding
  | Def         Name [(Name, RepType)] BindChain
  -- Exp
  -- Bind chain / result var
  | Let         [(Name, RepType, SimpleExp)] BindChain -- lazy let
  | LetRec      [(Name, RepType, SimpleExp)] BindChain -- lazy let with mutually recursive bindings
  | LetS        [(Name, RepType, SimpleExp)] BindChain -- strict let
  | Var         Name                                   -- Var is both terminator for BindChain and Simple Exp (let RHS)
  -- Simple Exp / let RHS
  | App         Name [Name]
  | Case        Name [Alt]
  | Con         Name [Name]
  | Lit         Lit
  | Closure     [Name] [(Name, RepType)] BindChain -- closure's captured variables ; arguments ; body
  -- Alt
  | Alt         Name Pat BindChain -- alt value (projected) ; case pattern ; alt body
  deriving (Generic, Data, Eq, Ord, Show)

data Lit
  = LInt64      Int64
  | LWord64     Word64
  | LFloat      Rational
  | LDouble     Rational    -- TODO: missing from GRIN
  | LBool       Bool
  | LChar       Char
  | LString     ByteString
  | LDataAddr   ByteString  -- TODO: missing from GRIN
  | LCodeAddr   ByteString  (Maybe Int) -- for std calls (for ghc backend) -- TODO: missing from GRIN
  | LNullAddr               -- TODO: missing from GRIN
  | LToken      ByteString
  -- special
  | LError      !ByteString  -- marks an error ; dead code elimination may elminate
  deriving (Generic, Data, Eq, Ord, Show)

data Pat
  = NodePat Name [Name]
  | LitPat  Lit
  | DefaultPat
  deriving (Generic, Data, Eq, Show, Ord)

makeBaseFunctor ''Ty
makeBaseFunctor ''Exp
makeStore ''Grin.Name
makeStore ''SimpleType
makeStore ''ExternalKind
makeStore ''StaticValue
makeStore ''StaticData
makeStore ''PrimRep
makeStore ''RepType
makeStore ''ConSpec
makeStore ''ConGroup
makeStore ''Lit
makeStore ''Pat
makeStore ''Ty
makeStore ''External
makeStore ''Exp
