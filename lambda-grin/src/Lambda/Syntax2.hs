{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, DeriveDataTypeable #-}
module Lambda.Syntax2
  ( module Lambda.Syntax2
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
import Lambda.Syntax
  ( SimpleType(..)
  , ExternalKind(..)
  , Lit(..)
  , Pat(..)
  , StaticData(..)
  , StaticValue(..)
  )

type Name = Grin.Name
type ConName = Name

data Ty
  = TyCon     Name ConName [Ty]
  | TyVar     Name
  | TySimple  Name SimpleType
  | TyFun     Name Ty [Ty]
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

type Alt        = Exp
type Def        = Exp
type Program    = Exp
type Bind       = Exp
type SimpleExp  = Exp

data Exp
  = Program     [External] [StaticData] [Def]
  -- Binding
  | Def         Name [Name] Bind
  -- Exp
  -- Bind chain / result var
  | Let         [(Name, SimpleExp)] Bind -- lazy let
  | LetRec      [(Name, SimpleExp)] Bind -- lazy let with mutually recursive bindings
  | LetS        [(Name, SimpleExp)] Bind -- strict let
  | Var         Name
  -- Simple Exp / let RHS
  | App         Name [Name]
  | Case        Name [Alt]
  | Con         Name [Name]
  | Lit         Lit
  | Closure     [Name] [Name] Bind -- closure's captured variables ; arguments ; body
  -- Alt
  | Alt         Name Pat Bind -- alt value (projected) ; case pattern ; alt body
  deriving (Generic, Data, Eq, Ord, Show)

makeBaseFunctor ''Exp
makeStore ''Ty
makeStore ''External
makeStore ''Exp
