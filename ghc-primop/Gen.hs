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

prettyFunction :: Pretty a => Name -> a -> [a] -> Doc
prettyFunction name ret args = pretty name <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ args ++ [ret]))

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



test = do
  i <- readInfo
  let Info ol el = i
      ops   = [e | e@PrimOpSpec{} <- el]
      mops  = [e | e@(PrimOpSpec _ n t _ _ _) <- el, isMonomorph t]
      tys   = [e | e@PrimTypeSpec{} <- el]
  --putStrLn " * Monomorph primops *"
  --putStrLn $ unlines [n ++ "\t\t\t" ++ show t | PrimOpSpec _ n t _ _ _ <- el, isMonomorph t]
  --putStrLn "\n-------------------------------\n"
  --putStrLn " * Polymorph primops *"
  --putStrLn $ unlines [n | PrimOpSpec _ n t _ d _ <- el, not $ isMonomorph t]
  printf "all ops: %d\nmonomorph ops: %d\npolymorph: %d\n" (length ops) (length mops) (length ops - length mops)
  putStrLn " * Monomorph primtypes *"
  putStrLn $ unlines [show t | PrimTypeSpec t _ _ <- tys, isMonomorph t]
  putStrLn " * Polymorph primtypes *"
  putStrLn $ unlines [show t | PrimTypeSpec t _ _ <- tys, not $ isMonomorph t]
  pure ()

  let defaults = ol
  -- NOTE: assumtion - default has_side_effects = False
  let primExtsMono = [e | PrimOpSpec _ n t _ _ _ <- el, isMonomorph t, e <- maybeToList $ cvtExternal n t False]
      primExtsPoly = [e | PrimOpSpec _ n t _ _ _ <- el, not $ isMonomorph t, e <- maybeToList $ cvtExternal n t False]
      primExts = primExtsMono ++ primExtsPoly
      ok = Set.fromList [unpackName $ eName e | e <- primExts]
  putStrLn " * Monomorph primtypes *"
  print $ length primExtsMono
  print $ L.prettyExternals primExtsMono
  putStrLn " * Polymorph primtypes *"
  print $ length primExtsPoly
  print $ L.prettyExternals primExtsPoly
  putStrLn " * Fails *"
  let fails = ["\t" ++ n{- ++ "\n\t" ++ show t-} | PrimOpSpec _ n t _ _ _ <- el, Set.notMember n ok]
  print $ length fails
  putStrLn . unlines $ "fails" : fails
  putStrLn " * Bad *"
  print $ L.prettyExternals [e | e@External{..} <- primExts, isBad eArgsType eRetType]
  putStrLn " * Good *"
  let showWidth :: Int -> Doc -> String
      showWidth w x = displayS (renderPretty 0.4 w x) ""
  let goodPrims = lines . showWidth 800 $ plain $ L.prettyExternals [e | e@External{..} <- primExts, not $ isBad eArgsType eRetType]
  let src = unlines $
        [ "{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}"
        , "module GHCPrimOps where"
        , ""
        , "import Grin.Grin"
        , "import Grin.TH"
        , ""
        , "primPrelude :: Program"
        , "primPrelude = [progConst|"
        ]
        ++ map ("  " ++ ) goodPrims ++
        ["  |]"]

  --putStrLn src
  writeFile "GHCPrimOps.hs" src
  let sections = [s | s@Section{} <- el]
  forM_ sections $ \Section{..} -> printf "section: %s\n" title

{-
TODO
  done - check for unknown type vars in result type: isBad
-}

cvtExternal :: String -> Ty -> Bool -> Maybe External
cvtExternal n t isEffectful = do
  (retTy : revArgsTy) <- reverse <$> cvtTy t
  pure External
    { eName       = packName n
    , eRetType    = retTy
    , eArgsType   = reverse revArgsTy
    , eEffectful  = isEffectful
    }

cvtTy :: Ty -> Maybe [L.Ty]
cvtTy = \case
  TyVar n -> Just [L.TyVar $ packName n]
  TyApp (TyCon n) []
    | Just t <- cvtType n -> Just [L.TySimple t]
  TyApp (TyCon n) args
    | Just xs <- concat <$> mapM cvtTy args
    -> Just [L.TyCon (packName n) xs]
  TyF t x
    | Just [t'] <- cvtTy t -> (t':) <$> cvtTy x
  TyUTup args
    | Just xs <- concat <$> mapM cvtTy args
    -> Just [L.TyCon (packName $ "Tup" ++ show (length args)) xs]
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
{-
 * Monomorph primtypes *
TyApp Char# []
TyApp Int# []
TyApp Word# []
TyApp Double# []
TyApp Float# []
TyApp Addr# []

  = T_Int64
  | T_Word64
  | T_Float
  | T_Double
  | T_Bool
  | T_Unit
  | T_String
  | T_Char
  | T_Addr

TyApp ByteArray# []
TyApp ArrayArray# []
TyApp RealWorld []
TyApp ThreadId# []
TyApp Compact# []
TyApp BCO# []
TyApp VECTOR []

 * Polymorph primtypes *
TyApp Array# [TyVar "a"]
TyApp MutableArray# [TyVar "s",TyVar "a"]
TyApp SmallArray# [TyVar "a"]
TyApp SmallMutableArray# [TyVar "s",TyVar "a"]
TyApp MutableByteArray# [TyVar "s"]
TyApp MutableArrayArray# [TyVar "s"]
TyApp MutVar# [TyVar "s",TyVar "a"]
TyApp TVar# [TyVar "s",TyVar "a"]
TyApp MVar# [TyVar "s",TyVar "a"]
TyApp State# [TyVar "s"]
TyApp Weak# [TyVar "b"]
TyApp StablePtr# [TyVar "a"]
TyApp StableName# [TyVar "a"]
TyApp Proxy# [TyVar "a"]

-}
tyVars :: L.Ty -> Set.Set Name
tyVars = \case
  L.TyCon _ a -> Set.unions $ map tyVars a
  L.TyVar n   -> Set.singleton n
  _ -> Set.empty

isBad :: [L.Ty] -> L.Ty -> Bool
isBad args res = not (Set.null $ Set.difference r a) where
  a = Set.unions $ map tyVars args
  r = tyVars res

{-
TyApp ByteArray# []
TyApp ArrayArray# []
TyApp Addr# []
TyApp RealWorld []
TyApp ThreadId# []
TyApp Compact# []
TyApp BCO# []
TyApp VECTOR []
-}

{-
data Ty
   = TyF    Ty Ty
   | TyC    Ty Ty -- We only allow one constraint, keeps the grammar simpler
   | TyApp  TyCon [Ty]
   | TyVar  TyVar
   | TyUTup [Ty]   -- unboxed tuples; just a TyCon really, 
                   -- but convenient like this
   deriving (Eq,Show)

type TyVar = String

data TyCon = TyCon String
           | SCALAR
           | VECTOR
           | VECTUPLE
           | VecTyCon String String
-}

-- ==========================================

{-
  generate GHC primop prelude module for Lambda
    done - handle attributes
    done - collect:
        done - unsupported primops
        done - supported primops group by sections
    - generate header
    - generate ghcUnsupportedPrimopSet
    - generate ghcPrimopPrelude
-}
{-
defaults
   has_side_effects = False
   out_of_line      = False   -- See Note Note [PrimOp can_fail and has_side_effects] in PrimOp
   can_fail         = False   -- See Note Note [PrimOp can_fail and has_side_effects] in PrimOp
   commutable       = False
   code_size        = { primOpCodeSizeDefault }
   strictness       = { \ arity -> mkClosedStrictSig (replicate arity topDmd) topRes }
   fixity           = Nothing
   llvm_only        = False
   vector           = []
-}

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
{-
  , envHasSideEffects   = attrBool opts "has_side_effects"
  , envCanFail          = attrBool opts "can_fail"
-}

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

{-
data Entry
    = PrimOpSpec { cons  :: String,      -- PrimOp name
                   name  :: String,      -- name in prog text
                   ty    :: Ty,          -- type
                   cat   :: Category,    -- category
                   desc  :: String,      -- description
                   opts  :: [Option] }   -- default overrides
    | PrimVecOpSpec { cons    :: String,    -- PrimOp name
                      name    :: String,    -- name in prog text
                      prefix  :: String,    -- prefix for generated names
                      veclen  :: Int,       -- vector length
                      elemrep :: String,    -- vector ElemRep
                      ty      :: Ty,        -- type
                      cat     :: Category,  -- category
                      desc    :: String,    -- description
                      opts    :: [Option] } -- default overrides
    | PseudoOpSpec { name  :: String,      -- name in prog text
                     ty    :: Ty,          -- type
                     desc  :: String,      -- description
                     opts  :: [Option] }   -- default overrides
    | PrimTypeSpec { ty    :: Ty,      -- name in prog text
                     desc  :: String,      -- description
                     opts  :: [Option] }   -- default overrides
    | PrimVecTypeSpec { ty    :: Ty,      -- name in prog text
                        prefix  :: String,    -- prefix for generated names
                        veclen  :: Int,       -- vector length
                        elemrep :: String,    -- vector ElemRep
                        desc  :: String,      -- description
                        opts  :: [Option] }   -- default overrides
    | Section { title :: String,         -- section title
                desc  :: String }        -- description
-}

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
        , "import Grin.Grin"
        , "import Grin.TH"
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
        [ "unsupported :: Set String"
        , "unsupported = Set.fromList"
        ] ++ ["  [ " ++ intercalate "\n  , " (map show envUnsupported) ++ "\n  ]"]

  writeFile "GHCPrimOps.hs" $ unlines $ header ++ primPrelude ++ unsupported
