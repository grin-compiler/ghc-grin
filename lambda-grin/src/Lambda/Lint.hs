{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Lambda.Lint where

import Text.Printf

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import Data.Functor.Foldable
import qualified Data.Foldable

import Lambda.Syntax
import Grin.Grin (isPrimName)
import Transformations.Util
import Lambda.Util

badPrimops :: Set Name
badPrimops = Set.fromList
  [ "_ghc_atomicModifyMutVar#"
  , "_ghc_catch#"
  , "_ghc_maskAsyncExceptions#"
  , "_ghc_maskUninterruptible#"
  , "_ghc_unmaskAsyncExceptions#"
  , "_ghc_atomically#"
  , "_ghc_catchRetry#"
  , "_ghc_catchSTM#"
  , "_ghc_mkWeak#"
  , "_ghc_clearCCS#"
  , "_ghc_broadcast#"
  , "_ghc_pack#"
  , "_ghc_unpack#"
  , "_ghc_insert#"
  , "_ghc_plus#"
  , "_ghc_minus#"
  , "_ghc_times#"
  , "_ghc_divide#"
  , "_ghc_quot#"
  , "_ghc_rem#"
  , "_ghc_negate#"
  , "_ghc_indexOffAddr#"
  , "_ghc_readOffAddr#"
  , "_ghc_writeOffAddr#"
  , "_ghc_indexArrayAs#"
  , "_ghc_readArrayAs#"
  , "_ghc_writeArrayAs#"
  , "_ghc_indexOffAddrAs#"
  , "_ghc_readOffAddrAs#"
  , "_ghc_writeOffAddrAs#"
  , "_ghc_indexStablePtrArray#"
  , "_ghc_indexWord8ArrayAsStablePtr#"
  , "_ghc_readStablePtrArray#"
  , "_ghc_readWord8ArrayAsStablePtr#"
  , "_ghc_indexStablePtrOffAddr#"
  , "_ghc_readStablePtrOffAddr#"
  , "_ghc_raise#"
  , "_ghc_raiseIO#"
  , "_ghc_retry#"
  , "_ghc_newMVar#"
  , "_ghc_finalizeWeak#"
  , "_ghc_getSpark#"
  , "_ghc_tagToEnum#"
  , "_ghc_addrToAny#"
  , "_ghc_mkApUpd0#"
  , "_ghc_unpackClosure#"
  , "_ghc_getApStackVal#"
  ]

lintLambda :: Program -> IO ()
lintLambda prg@(Program exts _) = do
  let Env{..} = test prg
      tab = ("  "++) . unpackName
      unknown = Set.difference envUse $ Map.keysSet envDef `Set.union` Set.fromList [eName | External{..} <- exts]
      unsupported = Set.intersection unknown badPrimops
  --printf "node pats:\n%s" . unlines . map tab $ Set.toList envCon
  
  printf "unknown:\n%s" . unlines . map tab $ Set.toList unknown
  printf "errors:\n%s" . unlines . map tab $ Set.toList envErr
  --printf "unused:\n%s" . unlines . map show $ Set.toList (Set.difference envDef envUse)
  let duplicates = [n | (n,i) <- Map.toList envDef, i > 1]
  printf "duplicates:\n%s" . unlines . map tab $ duplicates
  case Set.null unsupported of
    True  -> putStrLn "GHC primop: all ok"
    False -> printf "unsupported GHC primops:\n%s" . unlines . map tab $ Set.toList unsupported

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
    VarF _ name       -> env {envUse = Set.singleton name}
    AppF name e       -> mconcat $ env {envUse = Set.singleton name} : e
    -- def
    DefF name args e  -> env {envDef = addDefs $ name : args} <> e
    LetRecF binds e   -> mconcat [env {envDef = addDef name} <> a | (name, a) <- binds] <> e
    LetSF binds e     -> mconcat [env {envDef = addDef name} <> a | (name, a) <- binds] <> e
    LetF binds e      -> mconcat [env {envDef = addDef name} <> a | (name, a) <- binds] <> e
    LamF names e      -> env {envDef = addDefs names} <> e
    AltF (NodePat con args) e -> env {envDef = addDefs args, envCon = Set.singleton $ showTS (length args) <> "-" <> con} <> e
    -- err
    LitF (LError err) -> env {envErr = Set.singleton $ packName $ T.unpack err}
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

expSize :: Exp -> Int
expSize = cata folder where
  folder = \case
    VarF {} -> 1
    LitF {} -> 1
    e       -> succ $ Data.Foldable.sum e

programHistogram :: Program -> Map Int (Int, Name)
programHistogram (Program _ defs) = Map.unionsWith (\(i1,n1) (i2,n2) -> (i1 + i2, n1)) [Map.singleton (expSize d) (1, n) | d@(Def n _ _) <- defs]
