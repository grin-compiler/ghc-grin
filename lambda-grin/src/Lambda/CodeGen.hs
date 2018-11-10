{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings #-}
module Lambda.CodeGen (codegenGrin) where

import Text.Printf
import Control.Monad
import Control.Monad.State

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Foldable

import Lambda.Syntax
--import Lambda.PrimOps
import qualified Grin.Grin as G
import Transformations.Optimising.DeadProcedureElimination
import Transformations.GenerateEval

data Env
  = Env
  { _counter  :: Int
  , _arityMap :: Map Name Int
  , _whnfMap  :: Map Name Name
  }

type CG = State Env

uniq :: Name -> CG Name
uniq name = state (\env@Env{..} -> (name <> "." <> showTS  _counter, env {_counter = succ _counter}))

arity :: Map Name Int -> Name -> Maybe Int
arity = flip Map.lookup

genLit :: Lit -> G.Lit
genLit = \case
  LInt64  v -> G.LInt64 v
  LWord64 v -> G.LWord64 v
  LFloat  v -> G.LFloat v
  LBool   v -> G.LBool v
  _ -> G.LWord64 999 -- TODO
-- TODO
{-
data Lit
  | LChar   Char
  | LString ByteString
  -- special
  | LError  String  -- marks an error
  | LDummy  String  -- should be ignored
-}
genCPat :: Pat -> G.CPat
genCPat = \case
  NodePat name args -> G.NodePat (G.Tag G.C name) args
  LitPat  lit       -> G.LitPat (genLit lit)
  DefaultPat        -> G.DefaultPat
{-
apChain :: Mode -> G.Exp -> [Atom] -> CG G.Exp
apChain mode exp args = foldrM ap exp args where -- TODO: fix the order
  ap arg leftExp = do
    argWhnf <- uniq "arg"
    newArg <- genVal C arg
    case mode of
      pure $ G.EBind leftExp (G.Var argWhnf) (G.SApp "apply" [G.Var argWhnf, newArg])
-}
data Mode = C | E | R deriving (Eq, Ord, Show)

genVal :: Mode -> Atom -> CG G.Val
genVal mode = \case
  Var name  -> pure $ G.Var name -- TODO: handle C,E,R
  Lit lit   -> pure . G.Lit $ genLit lit
  x -> error $ printf "unsupported atom: %s in mode %s" (show x) (show mode)

gen :: Mode -> Exp -> CG G.Exp
gen mode e = gets _arityMap >>= \arityMap -> case e of
  Program defs      -> G.Program <$> mapM (gen mode) defs
  Def name args exp -> G.Def name args <$> gen mode exp

  Lit lit -> pure . G.SReturn . G.Lit $ genLit lit

  Con name args
    | mode == C
    -> G.SStore . G.ConstTagNode (G.Tag G.C name) <$> mapM (genVal C) args

    | mode == R
    -> G.SReturn . G.ConstTagNode (G.Tag G.C name) <$> mapM (genVal C) args

  Var name
    | mode == E || mode == R
    -> do
      gets $ Map.lookup name . _whnfMap >>= \case
        Just whnf -> pure $ G.SReturn $ G.Var whnf
        Nothing   -> pure $ G.SApp "eval" [G.Var name] -- TODO: handle functions

  -- TODO -----
  Var name
    -> do
      gets $ Map.lookup name . _whnfMap >>= \case
        Just whnf -> pure $ G.SReturn $ G.Var whnf
        Nothing   -> pure $ G.SApp "eval" [G.Var name] -- TODO: handle functions
  -------------

  -- TODO: build var name <--> location name map for suspended computations
  Let binds exp
    | mode == R
    -> foldr (\(name, e) rightExp -> G.EBind <$> gen C e <*> pure (G.Var name) <*> rightExp) (gen R exp) binds

  -- TODO -----
  LetRec binds exp
    | mode == R
    -> foldr (\(name, e) rightExp -> G.EBind <$> gen C e <*> pure (G.Var name) <*> rightExp) (gen R exp) binds
  -------------

  LetS binds exp
    | mode == R
    -> foldr (\(name, e) rightExp -> do
      leftExp <- gen E e
      -- track whnf
      modify' $ \env@Env{..} -> env {_whnfMap = Map.insert name name _whnfMap}
      G.EBind leftExp (G.Var name) <$> rightExp) (gen R exp) binds

  -- TODO -----
  LetS binds exp
    -> foldr (\(name, e) rightExp -> G.EBind <$> gen C e <*> pure (G.Var name) <*> rightExp) (gen R exp) binds
  Let binds exp
    -> foldr (\(name, e) rightExp -> G.EBind <$> gen C e <*> pure (G.Var name) <*> rightExp) (gen R exp) binds
  -------------

  App name args
    | argCount <- length args
    -> case arity arityMap name of

      Nothing
        -- NOTE: primops must be saturated
        | G.isPrimName name
        , mode == E || mode == R
        -> G.SApp name <$> mapM (genVal E) args

      -- unknown function ; generate apply chain
      {-
      Nothing
        | mode == C
        , [arg] <- args
        -> pure $ G.SStore $ G.ConstTagNode (G.Tag G.F "ap") [G.Var name, gen C arg]

        | mode == E
        -> apChain (G.SApp "eval" [G.Var name]) args
      -}
      -- known function
      Just ar -> case argCount `compare` ar of
        EQ
          | mode == C -> G.SStore . G.ConstTagNode (G.Tag G.F name) <$> mapM (genVal C) args
          | mode == E -> G.SApp name <$> mapM (genVal C) args
          | mode == R -> G.SApp name <$> mapM (genVal C) args

        LT
          | mode == C -> G.SStore  . G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) <$> mapM (genVal C) args
          | mode == E -> G.SReturn . G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) <$> mapM (genVal C) args
          | mode == R -> G.SReturn . G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) <$> mapM (genVal C) args

  -- TODO -----
        GT -> G.SApp name <$> mapM (genVal C) args
{-
        GT  -> let (funArgs, extraArgs) = splitAt ar args
               in apChain (G.SApp name $ map genAtom funArgs) extraArgs
-}
      _ -> G.SApp name <$> mapM (genVal C) args
  -------------

  Alt pat exp
    | mode == R
    -> G.Alt (genCPat pat) <$> gen R exp

  Case exp alts
    | mode == R
    -> do
      whnf <- uniq "value"
      scrutExp <- gen E exp
      -- track whnf
      case exp of
        Var name -> modify' $ \env@Env{..} -> env {_whnfMap = Map.insert name whnf _whnfMap}
        _ -> pure ()
      G.EBind scrutExp (G.Var whnf) . G.ECase (G.Var whnf) <$> mapM (gen R) alts -- TODO: handle name mapping for var name -> whnf value name

  x -> error $ printf "unsupported %s in mode: %s" (show x) (show mode)

{-
  TODO:
    done - unknown function: apply chain
    done - over application
    - generate pointer names for suspended computations
    done - generate eval function
    done - generate apply function
    - letrec and circular data structures
    done - primop
    - fill R, E, C
    - rewrite as Grin anamorphism
    done - higher order sample
    - circular data sample
-}

codegenGrin :: Program -> G.Program
codegenGrin exp = evalState (gen R exp) (Env 0 (buildArityMap exp) mempty)

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program defs) = Map.fromList [(name, length args) | Def name args _ <- defs]
buildArityMap _ = error "invalid expression, program expected"
