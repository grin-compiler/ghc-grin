{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Lambda.Lint where

import Text.Printf

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8

import Data.Functor.Foldable
import qualified Data.Foldable

import Lambda.Syntax
import Transformations.Util
import Lambda.Util

lintLambda :: Program -> IO ()
lintLambda prg@Program{..} = do
  let Env{..} = test prg
      tab     = ("  "++) . unpackName
      known   = Set.unions
                  [ Map.keysSet envDef
                  , Set.fromList [eName | External{..} <- pExternals]
                  , Set.fromList [sName | StaticData{..} <- pStaticData]
                  ]
      unknown = Set.difference envUse known
  --printf "node pats:\n%s" . unlines . map tab $ Set.toList envCon

  printf "unknown:\n%s" . unlines . map tab $ Set.toList unknown
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

addNames ns = Set.fromList ns

test = cata folder where
  folder = \case
    -- use
    VarF name         -> env {envUse = Set.singleton name}
    AppF name args    -> env {envUse = addNames $ name : args}
    -- def
    DefF name args e  -> env {envDef = addDefs $ name : map fst args} <> e
    LetRecF binds e   -> mconcat [env {envDef = addDef name} <> a | (name, _, a) <- binds] <> e
    LetSF binds e     -> mconcat [env {envDef = addDef name} <> a | (name, _, a) <- binds] <> e
    LetF binds e      -> mconcat [env {envDef = addDef name} <> a | (name, _, a) <- binds] <> e
    ClosureF v p e    -> env {envUse = addNames v, envDef = addDefs (map fst p)} <> e
    AltF a (NodePat con args) e -> env {envDef = addDefs (a : args), envCon = Set.singleton $ showTS (length args) <> "-" <> con} <> e
    -- err
    LitF (LError err) -> env {envErr = Set.singleton $ packName $ BS8.unpack err}
    e -> Data.Foldable.fold e

expSize :: Exp -> Int
expSize = cata folder where
  folder = \case
    VarF {} -> 1
    LitF {} -> 1
    e       -> succ $ Data.Foldable.sum e

programHistogram :: Program -> Map Int (Int, Name)
programHistogram Program{..} = Map.unionsWith (\(i1,n1) (i2,n2) -> (i1 + i2, n1)) [Map.singleton (expSize d) (1, n) | d@(Def n _ _) <- pDefinitions]
