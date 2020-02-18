{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Gen where

import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Text.Printf
import Parser
import Syntax
import Lambda.Syntax (External(..), SimpleType(..), packName, Name)
import qualified Lambda.Syntax as L
import qualified Lambda.Pretty as L
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), cat)
--import Numeric

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

readInfo :: IO Info
readInfo = do
  s <- readFile "primops.txt"
  case parse s of
     Left err -> error ("parse error at " ++ (show err))
     Right p_o_specs@(Info _ _)
        -> seq (sanityTop p_o_specs) (pure p_o_specs)

deriveNewName :: Name -> G Name
deriveNewName name = do
  i <- Map.findWithDefault 0 name <$> gets envNameCounter
  modify $ \env@Env{..} -> env {envNameCounter = Map.insert name (succ i) envNameCounter}
  {-
  let code    = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
      baseX a = showIntAtBase (length code) (code !!) a ""
  pure $ packName $ printf "%s.%s" name (baseX i)
  -}
  pure $ packName $ printf "%s.%d" name i

genName :: Name -> G Name
genName name = do
  m <- gets envNameMap
  case Map.lookup name m of
    Just n  -> pure n
    Nothing -> do
      n <- deriveNewName name
      modify $ \env@Env{..} -> env {envNameMap = Map.insert name n envNameMap}
      pure n

clearTyVarMap :: G ()
clearTyVarMap = modify $ \env -> env {envNameMap = mempty}

cvtExternal :: String -> Ty -> Bool -> ExceptT String G External
cvtExternal n t isEffectful = do
  (retTy, argsTy) <- cvtFunTy $ removeStateFromRetType t
  pure External
    { eName       = packName n
    , eRetType    = retTy
    , eArgsType   = argsTy
    , eEffectful  = isEffectful
    , eKind       = L.PrimOp
    }

mkUnboxedTuple :: [L.Ty] -> G L.Ty
mkUnboxedTuple args = do
  ut <- deriveNewName "t"
  pure $ case length args of
    0 -> L.TyCon ut (packName "GHC.Prim.(##)") []
    1 -> L.TyCon ut (packName "GHC.Prim.Unit#") args
    n -> L.TyCon ut (packName $ "GHC.Prim.(#" ++ replicate (max 0 $ n-1) ',' ++ "#)") args

isStateTy :: Ty -> Bool
isStateTy = \case
  TyApp (TyCon "State#") [_] -> True
  _ -> False

isTyF :: Ty -> Bool
isTyF = \case
  TyF{} -> True
  _     -> False

removeStateFromRetType :: Ty -> Ty
removeStateFromRetType = \case
  TyF a b
    | isTyF b -> TyF (removeStateFromRetType a) $ removeStateFromRetType b
    -- last ty item aka return type
    | otherwise -> TyF (removeStateFromRetType a) $ case b of
        -- remove returning state
        TyUTup args -> TyUTup [removeStateFromRetType t | t <- args, not $ isStateTy t]
        t | isStateTy t -> TyUTup []
          | otherwise   -> t

  t -> t

-- generate unique names for type nodes
cvtFunTy :: Ty -> ExceptT String G (L.Ty, [L.Ty])
cvtFunTy a = do
  lift clearTyVarMap
  ty <- cvtTy a
  case flattenFunTy ty of
    L.TyFun _ r args -> pure (r, args)
    t -> throwError $ "ivalid function type: " ++ show t

flattenFunTy :: L.Ty -> L.Ty
flattenFunTy = \case
  L.TyFun n1 (L.TyFun _n2 r args2) args1 -> flattenFunTy (L.TyFun n1 r (map flattenFunTy $ args1 ++ args2))
  L.TyCon a b args -> L.TyCon a b (map flattenFunTy args)
  t -> t

cvtTy :: Ty -> ExceptT String G L.Ty
cvtTy ty = case ty of
  TyVar n -> L.TyVar <$> lift (genName (packName n))
  TyApp (TyCon n) []
    | Just t <- cvtType n
    -> L.TySimple <$> lift (deriveNewName "t") <*> pure t
  TyApp (TyCon n) args -> do
    xs <- mapM cvtTy args
    tn <- lift (deriveNewName "t")
    pure $ L.TyCon tn (packName n) xs
  TyUTup args -> do
    xs <- mapM cvtTy args
    lift $ mkUnboxedTuple xs
  TyF a b -> do
    a2 <- cvtTy a
    b2 <- cvtTy b
    tn <- lift (deriveNewName "tf")
    pure $ L.TyFun tn b2 [a2]
  t -> throwError $ "unsupported type: " ++ show t

cvtType :: String -> Maybe SimpleType
cvtType = \case
  "Char#"   -> Just T_Char
  "Int#"    -> Just T_Int64
  "Word#"   -> Just T_Word64
  "Double#" -> Just T_Double
  "Float#"  -> Just T_Float
  "Addr#"   -> Just T_Addr
  _ -> Nothing

data Env
  = Env
  { envSections         :: [(String, [External], [(String, String)])] -- section, externals, unsupported ops
  , envDefaults         :: [Option]
  , envSectionTitle     :: String
  , envExternals        :: [External]
  , envUnsupported      :: [(String, String)] -- NOTE: primop name, message
  , envNameMap          :: Map Name Name
  , envNameCounter      :: Map Name Int
  }

emptyEnv :: [Option] -> Env
emptyEnv opts = Env
  { envSections         = []
  , envDefaults         = opts
  , envSectionTitle     = ""
  , envExternals        = []
  , envUnsupported      = []
  , envNameMap          = mempty
  , envNameCounter      = mempty
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
    err <- runExceptT $ cvtExternal name ty has_side_effects
    case err of
      Right e@External{..}
        -- | not (hasUnknownTyVar eArgsType eRetType) || Set.member name whiteList
        -> addExternal e
        -- | otherwise
        -- -> unsupportedPrimop name "unknown type parameters in the result type"
      Left msg
        -> unsupportedPrimop name msg

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
        ] ++ map tab (concat [comment title ++ (lines $ showWidth 800 $ plain $ L.prettyExternals2 exts) ++ [""] | (title, exts, _) <- envSections]) ++
        ["  |]\n"]

      unsupported =
        [ "unsupported :: Set.Set String"
        , "unsupported = Set.fromList"
        ] ++ ppUnsupported True envSections ++
        ["  ]"]

      ppUnsupported _ [] = []
      ppUnsupported isFirst ((_s,_,[]):xs) = ppUnsupported isFirst xs
      ppUnsupported isFirst ((s,_,l):xs) = ["\n  -- " ++ s] ++ goSection isFirst l ++ ppUnsupported False xs

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
