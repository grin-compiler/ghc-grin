{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.GrinCodeGen (codegenGrin) where

import Text.Printf
import Control.Monad
import Control.Monad.State

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Foldable

import Lambda.Syntax
import Lambda.Util
--import Lambda.PrimOps
import qualified Grin.Grin as G
import Transformations.GenerateEval
import Transformations.Util
import Transformations.Names hiding (mkNameEnv)

data Env
  = Env
  { _arityMap :: Map Name Int
  , _whnfMap  :: Map Name Name
  }

type CG = StateT Env NameM

uniq :: Name -> CG Name
uniq = lift . deriveNewName

arity :: Map Name Int -> Name -> Maybe Int
arity = flip Map.lookup

genLit :: Lit -> G.Lit
genLit = \case
  LInt64  v -> G.LInt64 v
  LWord64 v -> G.LWord64 v
  LFloat  v -> G.LFloat $ fromRational v
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

data Mode = C | E | R deriving (Eq, Ord, Show)

genVal :: Atom -> G.Val
genVal = \case
  Var isPtr name  -> G.Var name
  Lit lit   -> G.Lit $ genLit lit
  x -> error $ printf "unsupported atom: %s" (show x)

data CmdF a
  = G           (G.ExpF a)
  | GLetRec     [(Name, a)] a
  | ApplyChain  Name [G.Val]
  | FApChain    Name [G.Val]
  | OverApp     Name G.Exp (CmdF a)
  deriving (Functor, Foldable, Traversable)

bindSeq :: [(Maybe Name, G.Exp)] -> G.Exp -> G.Exp
bindSeq [] exp                          = exp
bindSeq ((Nothing, rhs) : binds) exp    = G.EBind rhs G.Unit $ bindSeq binds exp
bindSeq ((Just name, rhs) : binds) exp  = G.EBind rhs (G.Var name) $ bindSeq binds exp

bindSeq' :: [(Maybe Name, G.Exp)] -> G.Exp
bindSeq' [(_, rhs)]                 = rhs
bindSeq' ((Nothing, rhs) : binds)   = G.EBind rhs G.Unit $ bindSeq' binds
bindSeq' ((Just name, rhs) : binds) = G.EBind rhs (G.Var name) $ bindSeq' binds

genExp :: (Mode, Exp) -> CG G.Exp
genExp = hyloM folder builder where

  ap :: Name -> G.Val -> G.Val
  ap f x = G.ConstTagNode (G.Tag G.F "ap") [G.Var f, x]

  folder :: CmdF G.Exp -> CG G.Exp
  folder = \case
    G exp -> pure $ embed exp

    GLetRec binds exp -> do
      let hole  = G.ConstTagNode (G.Tag G.C "letrec-hole") []
          (names, rhsList) = unzip binds
      rhsNames <- mapM uniq names
      {-
      HINT:
        - create hole nodes
        - gen rhs in R
        - update holes with RHS values
      -}
      let cmds = [(Just name, G.SStore hole) | name <- names] ++
                 [(Just rhsName, rhs) | (rhsName, rhs) <- zip rhsNames rhsList] ++
                 [(Nothing, G.SUpdate name (G.Var rhsName)) | (name, rhsName) <- zip names rhsNames]
      pure $ bindSeq cmds exp

    ApplyChain name [] -> pure $ G.SApp "eval" [G.Var name]

    ApplyChain name args -> do
      whnfNames <- replicateM (length args) (uniq name)
      let (name0 : names) = whnfNames
      pure $ bindSeq ((Just name0, G.SApp "eval" [G.Var name]) :
        [ (Just bindName, G.SApp "apply" [G.Var argName, arg])
        | (arg, argName, bindName) <- zip3 args whnfNames names
        ]) (G.SApp "apply" [G.Var $ last whnfNames, last args])

    FApChain name [] -> pure $ G.SReturn G.Unit -- TODO: what is this??
    FApChain name [arg] -> pure . G.SStore $ ap name arg
    FApChain name args -> do
      let go n [(fn, x)] = [(Nothing, G.SReturn $ ap n x)]
          go n ((fn, x) : xs) = (Just fn, G.SStore (ap n x)) : go fn xs
      fNames <- replicateM (length args) (uniq name)
      pure $ bindSeq' $ go name (zip fNames args)

    OverApp name exp cmd -> G.EBind exp (G.Var name) <$> folder cmd

  builder :: (Mode, Exp) -> CG (CmdF (Mode, Exp))
  builder (mode, lambdaExp) = gets _arityMap >>= \arityMap -> case lambdaExp of
    AppExp {} -> error "grin codegen - illegal expression: AppExp"
    Lam {}    -> error "grin codegen - illegal expression: Lam"
    Program defs      -> pure . G $ G.ProgramF [(mode, d) | d <- defs]
    Def name args exp -> pure . G $ G.DefF name args (mode, exp)
    Alt pat exp       -> pure . G $ G.AltF (genCPat pat) (mode, exp)
    Lit lit           -> pure . G . G.SReturnF . G.Lit $ genLit lit
    Con name args
      | mode == C     -> pure . G . G.SStoreF  $ G.ConstTagNode (G.Tag G.C name) (map genVal args)
      | otherwise     -> pure . G . G.SReturnF $ G.ConstTagNode (G.Tag G.C name) (map genVal args)
    Var isPtr name
      | Just ar <- arity arityMap name -> pure . G $ G.SReturnF $ G.ConstTagNode (G.Tag (if ar == 0 then G.F else G.P ar) name) []
      | mode == E     -> pure . G $ G.SAppF "eval" [G.Var name]
      | otherwise     -> pure . G . G.SReturnF $ G.Var name

    Case (Var isPtr name) alts  -> pure . G $ G.ECaseF (G.Var name) [(mode, alt) | alt <- alts]

    Let [] exp                      -> builder (mode, exp)
    Let ((name, rhs) : binds) exp   -> pure . G $ G.EBindF (C, rhs) (G.Var name) (mode, Let binds exp)

    LetS [] exp                     -> builder (mode, exp)
    LetS ((name, rhs) : binds) exp  -> pure . G $ G.EBindF (E, rhs) (G.Var name) (mode, Let binds exp)

    LetRec binds exp -> pure $ GLetRec [(name, (R, rhs)) | (name, rhs) <- binds] (mode, exp)

    App name args
      | argCount <- length args
      -> case arity arityMap name of
        Nothing
          -- primops ; always saturated
          | G.isPrimName name -> pure . G $ G.SAppF name (map genVal args)

          -- unknown function
          | mode == E -> pure $ ApplyChain name (map genVal args)
          | otherwise -> pure $ FApChain name (map genVal args)

        -- known function
        Just ar -> case argCount `compare` ar of
          EQ
            | mode == C -> pure . G $ G.SStoreF $ G.ConstTagNode (G.Tag G.F name) (map genVal args)
            | otherwise -> pure . G $ G.SAppF name (map genVal args)

          LT
            | mode == C -> pure . G $ G.SStoreF  $ G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) (map genVal args)
            | otherwise -> pure . G $ G.SReturnF $ G.ConstTagNode (G.Tag (G.P $ ar - argCount) name) (map genVal args)

          GT -> do
            let (funArgs, extraArgs) = splitAt ar $ map genVal args
            fName <- uniq name
            case mode of
              C -> pure $ OverApp fName (G.SStore $ G.ConstTagNode (G.Tag G.F name) funArgs) (FApChain fName extraArgs)
              _ -> pure $ OverApp fName (G.SApp name funArgs) (ApplyChain fName extraArgs)


{-
  TODO:
    track for each var:
      - is pointer
      - is lit
      - is node
  NOTE:
    vars got binded by let

  Q: can def parameter be primitive value?
  A: Yes

-}


codegenGrin :: Program -> G.Program
codegenGrin exp = evalState (evalStateT (genExp (R, exp)) (Env (buildArityMap exp) mempty)) (mkNameEnv exp)

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program defs) = Map.fromList [(name, length args) | Def name args _ <- defs]
buildArityMap _ = error "invalid expression, program expected"

{-
  R, E  - return basic value or node
  C     - return pointer

phd:
  con   R
  var   E ; pointer i.e. from let R binding
  case  R
  case  E ; never used, ILLEGAL!
  letrec -- TODO
  let   R
  letS  R
  done - app   C ; known, saturated, == arity
  done - app   E ; known, saturated, == arity
  done - app   C ; known, non staurated , < arity
  done - app   E ; known, non staurated , < arity
  app   E ; known, over saturated, > arity (regular known + unknown)
  app   C ; unknown, store Fap
  app   R ; unknown, pure Fap
  app   E ; unknown, apply chain

questions:
  - lit: C E R
  - var: C E R ; can be pointer, node or lit

clear:
  program
  def
  alt
  appexp  - error
  lam     - error
-}
