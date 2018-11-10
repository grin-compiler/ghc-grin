{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Lambda.Lint where

import Text.Printf

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Functor.Foldable
import qualified Data.Foldable

import Lambda.Syntax
import Grin.Grin (isPrimName)
import Transformations.Util
import Lambda.Util

lintLambda :: Program -> IO ()
lintLambda prg = do
  let Env{..} = test prg
      tab = ("  "++) . unpackName
  --printf "node pats:\n%s" . unlines . map tab $ Set.toList envCon
  printf "unknown:\n%s" . unlines . map tab $ Set.toList (Set.difference envUse $ Map.keysSet envDef)
  printf "errors:\n%s" . unlines . map tab $ Set.toList envErr
  --printf "unused:\n%s" . unlines . map show $ Set.toList (Set.difference envDef envUse)
  let duplicates = [n | (n,i) <- Map.toList envDef, i > 1]
  printf "duplicates:\n%s" . unlines . map tab $ duplicates

data Env
  = Env
  { envDef  :: Map Name Int
  , envUse  :: Set Name
  , envCon  :: Set Name
  , envErr  :: Set Name
  }

instance Semigroup  Env where (Env a1 b1 c1 d1) <> (Env a2 b2 c2 d2) = Env (Map.unionWith (+) a1 a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)
instance Monoid     Env where mempty = env

env = Env
  { envDef  = mempty
  , envUse  = mempty
  , envCon  = mempty
  , envErr  = mempty
  }

addDef n = Map.singleton n 1
addDefs ns = Map.unionsWith (+) $ map addDef ns

test = cata folder where
  folder = \case
    -- use
    VarF name         -> env {envUse = Set.singleton name}
    AppF name e       -> mconcat $ env {envUse = Set.singleton name} : e
    -- def
    DefF name args e  -> env {envDef = addDefs $ name : args} <> e
    LetRecF binds e   -> mconcat [env {envDef = addDef name} <> a | (name, a) <- binds] <> e
    LetSF binds e     -> mconcat [env {envDef = addDef name} <> a | (name, a) <- binds] <> e
    LetF binds e      -> mconcat [env {envDef = addDef name} <> a | (name, a) <- binds] <> e
    LamF names e      -> env {envDef = addDefs names} <> e
    AltF (NodePat con args) e -> env {envDef = addDefs args, envCon = Set.singleton $ showTS (length args) <> "-" <> con} <> e
    -- err
    LitF (LError err) -> env {envErr = Set.singleton err}
    e -> Data.Foldable.fold e

{-
data Exp
  = Program     [Def]
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
  | Var         Name
  | Lit         Lit
  -- Alt
  | Alt         Pat Exp
  -- Extra
  | AppExp      Exp [Exp]         -- convenient for nested expressions i.e. lambdas
  | Lam         [Name] Exp

data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  | LChar   Char
  | LString ByteString
  -- special
  | LError  String  -- marks an error
  | LDummy  String  -- should be ignored

data Pat
  = NodePat Name [Name]
  | LitPat  Lit
  | DefaultPat
-}
