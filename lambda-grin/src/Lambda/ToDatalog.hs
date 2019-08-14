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

type Fact = (String, [Param])
type DL = Writer [Fact]

data Param
  = S String
  | N Name
  | I Int


toDatalog :: Program -> String
toDatalog prg = unlines . map prettyFact . execWriter $ convertProgram prg where
  prettyFact :: Fact -> String
  prettyFact (n, args) = n ++ "(" ++ intercalate ", " (map showParam args) ++ ")."

  showParam :: Param -> String
  showParam = \case
    S s -> show s
    I i -> show i
    N n -> show $ unpackName n

toFacts :: Program -> [(String, String)]
toFacts prg = map prettyFacts . Map.toList . Map.unionsWith (++) . map (\(f,a) -> Map.singleton f [a]) . execWriter $ convertProgram prg where
  factEq a b = fst a == fst b

  prettyFacts :: (String, [[Param]]) -> (String, String)
  prettyFacts (fname, l) = (fname ++ ".facts", unlines [intercalate "\t" (map showParam a) | a <- l])

  showParam :: Param -> String
  showParam = \case
    S s -> s
    I i -> show i
    N n -> unpackName n

convertExternal :: External -> DL ()
convertExternal e = tell
  [ ("ExternalFunction"
    , [ N $ eName e
      , S $ if eEffectful e then "effectful" else "pure"
      , I $ length $ eArgsType e
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
    tell [("FunctionParameter", [N n, I i, N p]) | (i,p) <- zip [0..] a]
    -- bind
    ret <- convertBind (CodeName n) b
    tell [("ReturnValue", [N n, N ret])]

data InstInfo
  = CodeName  Name  -- first instruction's parent
  | InstName  Name  -- next instruction's parent

emitInstSeq :: InstInfo -> (Name, a) -> DL InstInfo
emitInstSeq i (n,_) = case i of
  CodeName p  -> tell [("FirstInst", [N p, N n])] >> pure (InstName n)
  InstName p  -> tell [("NextInst", [N p, N n])] >> pure (InstName n)

convertBind :: InstInfo -> Bind -> DL Name
convertBind prevInst = \case
  Var n -> do
    pure n
  Let l b -> do
    forM_ l $ \(n,e) -> do
      tell [("EvalMode", [N n, S "lazy"])]
      convertSimpleExp n e
    i <- foldM emitInstSeq prevInst l
    convertBind i b
  LetS l b -> do
    forM_ l $ \(n,e) -> do
      tell [("EvalMode", [N n, S "strict"])]
      convertSimpleExp n e
    i <- foldM emitInstSeq prevInst l
    convertBind i b
  LetRec l b -> do
    forM_ l $ \(n,e) -> do
      tell [("EvalMode", [N n, S "lazy"])]
      convertSimpleExp n e
    case l of
      (x,_) : _ -> tell [("RecGroup", [N x, N n]) | (n,_) <- l]
      _ -> pure ()
    i <- foldM emitInstSeq prevInst l
    convertBind i b

convertSimpleExp :: Name -> Exp -> DL ()
convertSimpleExp result = \case
  Var n -> do
    tell [("Move", [N result, N n])]

  App n a -> do
    tell [("Call", [N result, N n])]
    tell [("CallArgument", [N result, I i, N p]) | (i,p) <- zip [0..] a]

  Con n a -> do
    tell [("Node", [N result, N n])]
    tell [("NodeArgument", [N result, I i, N p]) | (i,p) <- zip [0..] a]

  Lit l -> do
    tell [("Node", [N result, S $ litTag l])]
    tell [("NodeArgument", [N result, I 0, S $ show l])]

  Case n a -> do
    tell [("Case", [N result, N n])]
    mapM_ (convertAlt n) a

  Closure v p b -> do
    tell [("ClosureVariable",  [N result, I i, N x]) | (i,x) <- zip [0..] v]
    tell [("ClosureParameter", [N result, I i, N x]) | (i,x) <- zip [0..] p]
    -- bind
    ret <- convertBind (CodeName result) b
    tell [("ReturnValue", [N result, N ret])]

  e -> error $ "Simple Expression expected, got: " ++ show e

convertAlt :: Name -> Exp -> DL ()
convertAlt result = \case
  Alt a p b -> do
    case p of
      NodePat t l -> do
        tell [("Alt", [N result, N a, N t])]
        tell [("AltParameter", [N a, I i, N p]) | (i,p) <- zip [0..] l]
      LitPat l -> do
        tell [("Alt", [N result, N a, S $ litTag l])]
      DefaultPat -> do
        tell [("Alt", [N result, N a, S "default:"])]
    -- bind
    ret <- convertBind (CodeName a) b
    tell [("ReturnValue", [N a, N ret])]

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
