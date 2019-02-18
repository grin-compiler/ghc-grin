{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Gen where

import Control.Monad.State
import Control.Monad
import Data.Maybe
import Text.Printf
import Parser
import Syntax
import Lambda.Syntax (External(..), SimpleType(..), packName, unpackName, Name)
import qualified Lambda.Syntax as L
import qualified Lambda.Pretty as L
import Text.Show.Pretty (ppShow)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Data.Char
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

readInfo :: IO Info
readInfo = do
  s <- readFile "primops.txt"
  case parse s of
     Left err -> error ("parse error at " ++ (show err))
     Right p_o_specs@(Info _ _)
        -> seq (sanityTop p_o_specs) (pure p_o_specs)

isMonomorph :: Ty -> Bool
isMonomorph = \case
  TyF a b -> all isMonomorph [a, b]
  TyC a b -> all isMonomorph [a, b]
  TyApp _ args -> null args
  TyVar {} -> False
  TyUTup a -> all isMonomorph a

cvtExternal :: String -> Ty -> Bool -> Either String External
cvtExternal n t isEffectful = do
  res <- reverse <$> firstOrderFunTy t
  let (retTy' : revArgsTy') = res
  retTy <- cvtTy $ prepareRetType retTy'
  revArgsTy <- mapM cvtTy $ filter (not . isStateTy) revArgsTy'
  pure External
    { eName       = packName n
    , eRetType    = retTy
    , eArgsType   = reverse revArgsTy
    , eEffectful  = isEffectful
    }

mkUnboxedTuple :: [L.Ty] -> L.Ty
mkUnboxedTuple args = case length args of
  1 -> L.TyCon (packName "GHC.Prim.Unit#") args
  n -> L.TyCon (packName $ "GHC.Prim.(#" ++ replicate (max 0 $ n-1) ',' ++ "#)") args

isStateTy :: Ty -> Bool
isStateTy = \case
  TyApp (TyCon "State#") [_] -> True
  _ -> False

prepareRetType :: Ty -> Ty
prepareRetType = \case
  TyUTup args -> TyUTup [t | t <- args, not $ isStateTy t]
  t | isStateTy t -> TyUTup []
    | otherwise   -> t

firstOrderTy :: Ty -> Either String Ty
firstOrderTy = \case
  TyApp (TyCon n) args -> do
    xs <- mapM firstOrderTy args
    pure $ TyApp (TyCon n) xs

  TyVar n
    -> Right $ TyVar n

  TyUTup args -> do
    xs <- mapM firstOrderTy args
    pure $ TyUTup xs

  TyF{} -> Left "higher order type"
  TyC{} -> Left "consraint type"
  t -> Left $ "unsupported type: " ++ show t

firstOrderFunTy :: Ty -> Either String [Ty]
firstOrderFunTy = \case
  TyF t x -> (:)   <$> firstOrderTy t <*> firstOrderFunTy x
  t       -> (:[]) <$> firstOrderTy t

cvtTy :: Ty -> Either String L.Ty
cvtTy ty = case adjustTy ty of
  TyVar n -> Right $ L.TyVar $ packName n
  TyApp (TyCon n) []
    | Just t <- cvtType n -> Right $ L.TySimple t
  TyApp (TyCon n) args
    | Right xs <- mapM cvtTy args
    -> Right $ L.TyCon (packName n) xs
  TyUTup args
    | Right xs <- mapM cvtTy args
    -> Right $ mkUnboxedTuple xs
  t -> Left $ "unsupported type: " ++ show t


-- removes state variable from primitive types
adjustTy :: Ty -> Ty
adjustTy = \case
  TyApp (TyCon "TVar#")               [s, a]  -> TyApp (TyCon "TVar#")              [a]
  TyApp (TyCon "MVar#")               [s, a]  -> TyApp (TyCon "MVar#")              [a]
  TyApp (TyCon "MutVar#")             [s, a]  -> TyApp (TyCon "MutVar#")            [a]
  TyApp (TyCon "MutableArray#")       [s, a]  -> TyApp (TyCon "MutableArray#")      [a]
  TyApp (TyCon "SmallMutableArray#")  [s, a]  -> TyApp (TyCon "SmallMutableArray#") [a]
  TyApp (TyCon "MutableArrayArray#")  [s]     -> TyApp (TyCon "MutableArrayArray#") []
  TyApp (TyCon "MutableByteArray#")   [s]     -> TyApp (TyCon "MutableByteArray#")  []
  t -> t

cvtType :: String -> Maybe SimpleType
cvtType = \case
  "Char#"   -> Just T_Char
  "Int#"    -> Just T_Int64
  "Word#"   -> Just T_Word64
  "Double#" -> Just T_Double
  "Float#"  -> Just T_Float
  "Addr#"   -> Just T_Addr
  _ -> Nothing

tyVars :: L.Ty -> Set.Set Name
tyVars = \case
  L.TyCon _ a -> Set.unions $ map tyVars a
  L.TyVar n   -> Set.singleton n
  _ -> Set.empty

-- check for unknown type vars in result type
isBad :: [L.Ty] -> L.Ty -> Bool
isBad args res = not (Set.null $ Set.difference r a) where
  a = Set.unions $ map tyVars args
  r = tyVars res
isBad _ _ = False

data Env
  = Env
  { envSections         :: [(String, [External], [(String, String)])] -- section, externals, unsupported ops
  , envDefaults         :: [Option]
  , envSectionTitle     :: String
  , envExternals        :: [External]
  , envUnsupported      :: [(String, String)] -- NOTE: primop name, message
  }

emptyEnv :: [Option] -> Env
emptyEnv opts = Env
  { envSections         = []
  , envDefaults         = opts
  , envSectionTitle     = ""
  , envExternals        = []
  , envUnsupported      = []
  }

type G = State Env

attr :: [Option] -> (Option -> Maybe a) -> G a
attr opts f = do
  defaults <- gets envDefaults
  pure $ head $ catMaybes $ map f (opts ++ defaults)

attrBool :: String -> [Option] -> G Bool
attrBool name opts = attr opts $ \case
  OptionFalse n | n == name -> Just False
  OptionTrue  n | n == name -> Just True
  _ -> Nothing

unsupportedPrimop :: String -> String -> G ()
unsupportedPrimop name msg = modify $ \s@Env{..} -> s {envUnsupported = envUnsupported ++ [(name, msg)]}

addExternal :: External -> G ()
addExternal e = modify $ \s@Env{..} -> s {envExternals = envExternals ++ [e]}

flushSection :: G ()
flushSection = do
  exts <- gets envExternals
  errs <- gets envUnsupported
  if null exts && null errs
    then modify $ \s@Env{..} -> s {envExternals = [], envUnsupported = []}
    else modify $ \s@Env{..} -> s {envExternals = [], envUnsupported = [], envSections = envSections ++ [(envSectionTitle, exts, errs)]}

setSection :: String -> G ()
setSection title = modify $ \s@Env{..} -> s {envSectionTitle = title}

visitEntry :: Entry -> G ()
visitEntry = \case
  PrimVecOpSpec{..} -> unsupportedPrimop name "SIMD is not supported yet"
  PseudoOpSpec{..}  -> unsupportedPrimop name "pseudo ops are not supported"

  PrimOpSpec{..} -> do
    has_side_effects <- attrBool "has_side_effects" opts
    case cvtExternal name ty has_side_effects of
      Right e@External{..}
        | not (isBad eArgsType eRetType)
        -> addExternal e
        | otherwise
        -> unsupportedPrimop name "unknown type parameters in the result type"
      Left err
        -> unsupportedPrimop name err

  Section{..} -> do
    flushSection
    setSection title

  PrimTypeSpec{}    -> pure ()
  PrimVecTypeSpec{} -> pure ()

processEntries :: [Entry] -> G ()
processEntries ents = do
  mapM_ visitEntry ents
  flushSection

showWidth :: Int -> Doc -> String
showWidth w x = displayS (renderPretty 0.4 w x) ""

genGHCPrimOps :: IO ()
genGHCPrimOps = do
  i <- readInfo
  let Info opts ents = i
      Env{..} = execState (processEntries ents) (emptyEnv opts)
  let header =
        [ "{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}"
        , "module Lambda.GHCPrimOps where"
        , ""
        , "import qualified Data.Set as Set"
        , "import Lambda.Syntax"
        , "import Lambda.TH"
        , ""
        ]

      tab l
        | all isSpace l = ""
        | otherwise     = "  " ++ l

      comment t =
        [ "{-"
        , "  " ++ t
        , "-}"
        ]

      primPrelude =
        [ "primPrelude :: Program"
        , "primPrelude = [progConst|"
        ] ++ map tab (concat [comment title ++ (lines $ showWidth 800 $ plain $ L.prettyExternals exts) ++ [""] | (title, exts, _) <- envSections]) ++
        ["  |]\n"]

      unsupported =
        [ "unsupported :: Set.Set String"
        , "unsupported = Set.fromList"
        ] ++ go True envSections ++
        ["  ]"]

      go _ [] = []
      go isFirst ((s,_,[]):xs) = go isFirst xs
      go isFirst ((s,_,l):xs) = ["\n  -- " ++ s] ++ goSection isFirst l ++ go False xs

      goSection _ [] = []
      goSection True ((n,msg):xs)   = ["  [ " ++ (take 40 $ show n ++ repeat ' ') ++ " -- " ++ msg] ++ goSection False xs
      goSection False ((n,msg):xs)  = ["  , " ++ (take 40 $ show n ++ repeat ' ') ++ " -- " ++ msg] ++ goSection False xs

  writeFile "GHCPrimOps.hs" $ unlines $ header ++ primPrelude ++ unsupported

{-
  generate GHC primop prelude module for Lambda
    done - handle attributes
    done - collect:
        done - unsupported primops
        done - supported primops group by sections
    done - generate header
    done - generate ghcUnsupportedPrimopSet
    done - generate ghcPrimopPrelude
    no - transform primop names to   GHC:op_name
    no - transform primtype names to GHC:type_name
    done - filter out void representations from the result ; State#
    done - map unboxed tuples properly
-}
{-
  STG state convention:
    - passes as arg
    - does not return ; filtered out from the result
-}
