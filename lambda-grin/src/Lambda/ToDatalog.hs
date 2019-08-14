{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.ToDatalog where

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Writer hiding (Alt)
import Control.Monad.State
import Data.List (intercalate, groupBy)

import Lambda.Syntax2

import Transformations.Util
import Lambda.Util

type Fact = (String, [String])
type DL = Writer [Fact]

{-
  = Program     [External] [Def]
  -- Binding
  | Def         Name [Name] Bind
  -- Exp
  -- Bind chain / result var
  | Let         [(Name, Exp)] Bind -- lazy let
  | LetRec      [(Name, Exp)] Bind -- lazy let with mutually recursive bindings
  | LetS        [(Name, Exp)] Bind -- strict let
  | Var         Name
  -- Simple Exp / let RHS
  | App         Name [Name]
  | Case        Name [Alt]
  | Con         Name [Name]
  | Lit         Lit
  | Closure     [Name] [Name] Bind -- closure's captured variables ; arguments ; body
  -- Alt
  | Alt         Name Pat Bind -- alt value (projected) ; case pattern ; alt body
-}

toDatalog :: Program -> String
toDatalog prg = unlines . map prettyFact . execWriter $ convertProgram prg

toFacts :: Program -> [(String, String)]
toFacts prg = map prettyFacts . Map.toList . Map.unionsWith (++) . map (\(f,a) -> Map.singleton f $ [intercalate "\t" a]) . execWriter $ convertProgram prg where
  factEq a b = fst a == fst b

  prettyFacts :: (String, [String]) -> (String, String)
  prettyFacts (fname, l) = (fname ++ ".facts", unlines l)

prettyFact :: Fact -> String
prettyFact (n, args) = n ++ "(" ++ intercalate ", " args ++ ")."

convertExternal :: External -> DL ()
convertExternal e = tell
  [ ("ExternalFunction"
    , [ show $ unpackName $ eName e
      , show $ if eEffectful e then "effectful" else "pure"
      , show $ length $ eArgsType e
      ]
    )
  ]

convertProgram :: Exp -> DL ()
convertProgram = \case
  Program e d -> do
    mapM_ convertExternal e
    mapM_ convertDef d

convertDef :: Exp -> DL ()
convertDef = \case
  Def n a b -> do
    tell [("FunctionParameter", [show $ unpackName n, show i, show $ unpackName p]) | (i,p) <- zip [0..] a]
    -- bind
    ret <- convertBind (CodeName n) b
    tell [("ReturnValue", [show $ unpackName n, show $ unpackName ret])]

data InstInfo
  = CodeName  Name  -- first instruction's parent
  | InstName  Name  -- next instruction's parent

emitInstSeq :: InstInfo -> (Name, a) -> DL InstInfo
emitInstSeq i (n,_) = case i of
  CodeName p  -> tell [("FirstInst", [show $ unpackName p, show $ unpackName n])] >> pure (InstName n)
  InstName p  -> tell [("NextInst", [show $ unpackName p, show $ unpackName n])] >> pure (InstName n)

convertBind :: InstInfo -> Bind -> DL Name
convertBind prevInst = \case
  Var n -> do
    pure n
  Let l b -> do
    forM_ l $ \(n,e) -> do
      tell [("EvalMode", [show $ unpackName n, show "lazy"])]
      convertSimpleExp n e
    i <- foldM emitInstSeq prevInst l
    convertBind i b
  LetS l b -> do
    forM_ l $ \(n,e) -> do
      tell [("EvalMode", [show $ unpackName n, show "strict"])]
      convertSimpleExp n e
    i <- foldM emitInstSeq prevInst l
    convertBind i b
  LetRec l b -> do
    forM_ l $ \(n,e) -> do
      tell [("EvalMode", [show $ unpackName n, show "lazy"])]
      convertSimpleExp n e
    case l of
      (x,_) : _ -> tell [("RecGroup", [show $ unpackName x, show $ unpackName n]) | (n,_) <- l]
      _ -> pure ()
    i <- foldM emitInstSeq prevInst l
    convertBind i b

convertSimpleExp :: Name -> Exp -> DL ()
convertSimpleExp result = \case
  Var n -> do
    tell [("Move", [show $ unpackName result, show $ unpackName n])]

  App n a -> do
    tell [("Call", [show $ unpackName result, show $ unpackName n])]
    tell [("CallArgument", [show $ unpackName result, show i, show $ unpackName p]) | (i,p) <- zip [0..] a]

  Con n a -> do
    tell [("Node", [show $ unpackName result, show $ unpackName n])]
    tell [("NodeArgument", [show $ unpackName result, show i, show $ unpackName p]) | (i,p) <- zip [0..] a]

  Lit l -> do
    tell [("Node", [show $ unpackName result, show $ litTag l])]
    tell [("NodeArgument", [show $ unpackName result, "0", show $ show l])]

  Case n a -> do
    tell [("Case", [show $ unpackName result, show $ unpackName n])]
    mapM_ (convertAlt n) a

  Closure v p b -> do
    tell [("ClosureVariable",  [show $ unpackName result, show i, show $ unpackName x]) | (i,x) <- zip [0..] v]
    tell [("ClosureParameter", [show $ unpackName result, show i, show $ unpackName x]) | (i,x) <- zip [0..] p]
    -- bind
    ret <- convertBind (CodeName result) b
    tell [("ReturnValue", [show $ unpackName result, show $ unpackName ret])]

  e -> error $ "Simple Expression expected, got: " ++ show e

convertAlt :: Name -> Exp -> DL ()
convertAlt result = \case
  Alt a p b -> do
    case p of
      NodePat t l -> do
        tell [("Alt", [show $ unpackName result, show $ unpackName a, show $ unpackName t])]
        tell [("AltParameter", [show $ unpackName a, show i, show $ unpackName p]) | (i,p) <- zip [0..] l]
      LitPat l -> do
        tell [("Alt", [show $ unpackName result, show $ unpackName a, show $ litTag l])]
      DefaultPat -> do
        tell [("Alt", [show $ unpackName result, show $ unpackName a, show "default:"])]
    -- bind
    ret <- convertBind (CodeName a) b
    tell [("ReturnValue", [show $ unpackName a, show $ unpackName ret])]

litTag :: Lit -> String
litTag l = "lit:" ++ case l of
  LInt64{}      -> "LInt64"
  LWord64{}     -> "LWord64"
  LFloat{}      -> "LFloat"
  LDouble{}     -> "LDouble"
  LBool{}       -> "LBool"
  LChar{}       -> "LChar"
  LString{}     -> "LString"
  LLabelAddr{}  -> "LLabelAddr"
  LNullAddr{}   -> "LNullAddr"
  LError{}      -> "LError"
