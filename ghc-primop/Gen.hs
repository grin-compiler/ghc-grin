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

cvtExternal :: String -> Ty -> Bool -> Maybe External
cvtExternal n t isEffectful = do
  (retTy' : revArgsTy') <- reverse <$> firstOrderFunTy t
  retTy <- cvtTy $ prepareRetType retTy'
  revArgsTy <- mapM cvtTy revArgsTy'
  pure External
    { eName       = packName n
    , eRetType    = retTy
    , eArgsType   = reverse revArgsTy
    , eEffectful  = isEffectful
    }

mkUnboxedTuple :: [L.Ty] -> L.Ty
mkUnboxedTuple args = case length args of
  1 -> L.TyCon (packName "GHC.Prim.Unit#") args
  n -> L.TyCon (packName $ "GHC.Prim.(#" ++ replicate n ',' ++ "#)") args


isStateTy :: Ty -> Bool
isStateTy = \case
  TyApp (TyCon "State#") [_] -> True
  _ -> False

prepareRetType :: Ty -> Ty
prepareRetType = \case
  TyUTup args -> TyUTup [t | t <- args, not $ isStateTy t]
  t | isStateTy t -> TyUTup []
    | otherwise   -> t


firstOrderFunTy :: Ty -> Maybe [Ty]
firstOrderFunTy = \case
  TyF t x
    | Just [t'] <- firstOrderFunTy t
    -> (t':) <$> firstOrderFunTy x

  TyApp (TyCon n) args
    | Just xs <- concat <$> mapM firstOrderFunTy args
    -> Just [TyApp (TyCon n) xs]

  TyVar n
    -> Just [TyVar n]

  TyUTup args
    | Just xs <- concat <$> mapM firstOrderFunTy args
    -> Just [TyUTup xs]

  _ -> Nothing

cvtTy :: Ty -> Maybe L.Ty
cvtTy = \case
  TyVar n -> Just $ L.TyVar $ packName n
  TyApp (TyCon n) []
    | Just t <- cvtType n -> Just $ L.TySimple t
  TyApp (TyCon n) args
    | Just xs <- mapM cvtTy args
    -> Just $ L.TyCon (packName n) xs
  TyUTup args
    | Just xs <- mapM cvtTy args
    -> Just $ mkUnboxedTuple xs
  _ -> Nothing

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

data Env
  = Env
  { envUnsupported      :: [String]
  , envSections         :: [(String, [External])]
  , envDefaults         :: [Option]
  , envSectionTitle     :: String
  , envExternals        :: [External]
  }

emptyEnv :: [Option] -> Env
emptyEnv opts = Env
  { envUnsupported      = []
  , envSections         = []
  , envDefaults         = opts
  , envSectionTitle     = ""
  , envExternals        = []
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

unsupportedPrimop :: String -> G ()
unsupportedPrimop name = modify $ \s@Env{..} -> s {envUnsupported = name : envUnsupported}

addExternal :: External -> G ()
addExternal e = modify $ \s@Env{..} -> s {envExternals = envExternals ++ [e]}

flushSection :: G ()
flushSection = do
  exts <- gets envExternals
  if null exts
    then modify $ \s@Env{..} -> s {envExternals = []}
    else modify $ \s@Env{..} -> s {envExternals = [], envSections = envSections ++ [(envSectionTitle, exts)]}

setSection :: String -> G ()
setSection title = modify $ \s@Env{..} -> s {envSectionTitle = title}

visitEntry :: Entry -> G ()
visitEntry = \case
  PrimVecOpSpec{..} -> unsupportedPrimop name
  PseudoOpSpec{..}  -> unsupportedPrimop name

  PrimOpSpec{..} -> do
    has_side_effects <- attrBool "has_side_effects" opts
    case cvtExternal name ty has_side_effects of
      Just e@External{..}
        | not (isBad eArgsType eRetType)
        -> addExternal e
      _ -> unsupportedPrimop name

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
        , "module GHCPrimOps where"
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
        ] ++ map tab (concat [comment title ++ (lines $ showWidth 800 $ plain $ L.prettyExternals exts) | (title, exts) <- envSections]) ++
        ["  |]\n"]

      unsupported =
        [ "unsupported :: Set.Set String"
        , "unsupported = Set.fromList"
        ] ++ ["  [ " ++ intercalate "\n  , " (map show envUnsupported) ++ "\n  ]"]

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
