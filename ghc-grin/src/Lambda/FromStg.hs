{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards, OverloadedStrings #-}
module Lambda.FromStg (codegenLambda) where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.State.Strict
import Text.Printf
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Control.Monad.Trans.Maybe

-- GHC Dump
import qualified Stg.Syntax as C
import qualified Stg.Pretty as C
import qualified Text.PrettyPrint.ANSI.Leijen as P

-- Lambda
import Lambda.Syntax
import Lambda.Util

import qualified Lambda.GHCPrimOps as GHCPrim

type CG = StateT Env IO

data Env
  = Env
  { moduleName    :: !Name
  , externals     :: !(Map Name External)
  , defs          :: ![Def]
  , staticData    :: ![StaticData]

  -- bind chain handling
  , commands      :: ![Cmd]
  , commandStack  :: ![[Cmd]]

  -- name handling
  , namePool      :: !(Map Name Int)
  , nameSet       :: !(Set Name)
  , binderNameMap :: !(Map Name Name)
  }

-- utility

addExternal :: External -> CG ()
addExternal ext = modify $ \env@Env{..} -> env {externals = Map.insert (eName ext) ext externals}

addDef :: Def -> CG ()
addDef d = modify' $ \env -> env {defs = d : defs env}

addStaticData :: StaticData -> CG ()
addStaticData sd = modify' $ \env -> env {staticData = sd : staticData env}

-- data con handling

genDataConName :: C.DataCon -> Name
genDataConName C.DataCon{..} = packName . BS8.unpack $ C.getModuleName dcModule <> "." <> dcName

-- name handling

deriveNewName :: Name -> CG Name
deriveNewName name = do
  {-
    - renerates unique name like: my_name.1
    - does not add to substitution map
  -}
  (newName, conflict) <- state $ \env@Env{..} ->
    let idx = Map.findWithDefault 0 name namePool
        new = packName $ printf "%s.%d" name idx
    in  ( (new, Set.member new nameSet)
        , env {namePool = Map.insert name (succ idx) namePool, nameSet = Set.insert new nameSet}
        )
  if conflict
    then deriveNewName name
    else pure newName

-- maps GHC unique binder names to unique lambda names
genName :: C.Binder -> CG Name
genName b = do
  let originalName = packName . BS8.unpack . C.binderUniqueName $ b
  Env{..} <- get
  case Map.lookup originalName binderNameMap of
    Just name -> pure name
    Nothing -> case Set.member originalName nameSet of
      False -> do
        -- case: new GHC binder name (without name conflict)
        modify $ \env@Env{..} -> env {nameSet = Set.insert originalName nameSet, binderNameMap = Map.insert originalName originalName binderNameMap}
        pure originalName
      True -> do
        -- case: new GHC binder name (with name conflict)
        name <- deriveNewName originalName
        modify $ \env@Env{..} -> env {binderNameMap = Map.insert originalName name binderNameMap}
        pure name

genBinder :: C.Binder -> CG (Name, RepType)
genBinder b = (,) <$> genName b <*> pure (convertType $ C.binderType b)

-- rep type conversion

-- TODO: fix name decoding
{-

toLambda: /home/csaba/haskell/grin-compiler/ghc-grin/ghc-8.6.2/libraries/base/dist-install/build/Control/Exception/Base.stgbin
unknown DataCon representation:
  Binder
    { binderName        = "(#,#)"
    , binderId          = BinderId 86
    , binderType        = TypeInfo
                            { tRep    = Nothing
                            , tTyCon  = Nothing
                            , tType   = "forall a b. a -> b -> (# a, b #)"
                            }
    , binderModule      = ModuleName { getModuleName = "GHC.Prim" }
    , binderIsTop       = True
    , binderIsExported  = True
    }

  TyCon
    { tcName      = "(#,#)"
    , tcId        = TyConId 54
    , tcDataCons  =
        [ Binder
            { binderName        = "(#,#)"
            , binderId          = BinderId 86
            , binderType        = TypeInfo {tRep = Nothing, tTyCon = Nothing, tType = "forall a b. a -> b -> (# a, b #)"}
            , binderModule      = ModuleName {getModuleName = "GHC.Prim"}
            , binderIsTop       = True
            , binderIsExported  = True
            }
        ]
    }
-}
isUnboxedTuple :: BS8.ByteString -> Bool
isUnboxedTuple "GHC.Prim.Unit#" = True
isUnboxedTuple name             = BS8.isPrefixOf "GHC.Prim.(#" name

convertType :: C.Type -> RepType
convertType = \case
  C.SingleValue r   -> SingleValue $ getPrimRep r
  C.UnboxedTuple l  -> UnboxedTuple $ map getPrimRep l
  C.PolymorphicRep  -> PolymorphicRep

getPrimRep :: C.PrimRep -> PrimRep
getPrimRep = \case
  -- TODO
  C.Int8Rep     -> Int64Rep
  C.Int16Rep    -> Int64Rep
  C.Int32Rep    -> Int64Rep
  C.Int64Rep    -> Int64Rep
  C.IntRep      -> Int64Rep
  C.Word8Rep    -> Word64Rep
  C.Word16Rep   -> Word64Rep
  C.Word32Rep   -> Word64Rep
  C.Word64Rep   -> Word64Rep
  C.WordRep     -> Word64Rep
  -- OK from here
  C.AddrRep     -> AddrRep
  C.FloatRep    -> FloatRep
  C.DoubleRep   -> DoubleRep
  C.VoidRep     -> VoidRep
  C.LiftedRep   -> LiftedRep
  C.UnliftedRep -> UnliftedRep

-- external type conversion

getType :: C.Name -> C.PrimRep -> Maybe SimpleType
getType t = \case
  C.Int8Rep   -> Just T_Int64
  C.Int16Rep  -> Just T_Int64
  C.Int32Rep  -> Just T_Int64
  C.Int64Rep  -> Just T_Int64
  C.IntRep    -> Just T_Int64
  C.Word8Rep  -> Just T_Word64
  C.Word16Rep -> Just T_Word64
  C.Word32Rep -> Just T_Word64
  C.Word64Rep -> Just T_Word64
  C.WordRep   -> Just T_Word64
  C.AddrRep   -> Just T_Addr
  C.FloatRep  -> Just T_Float
  C.DoubleRep -> Just T_Double
  C.VoidRep   -> Just (T_Token t)
  -- NOTE:
  --  1. FFI does not support thunks and boxed types
  --  2. VecRep is not supported yet
  _ -> Nothing

showArgType :: C.Arg -> String
showArgType = \case
  C.StgLitArg l -> show $ getLitType l
  C.StgVarArg b -> show $ C.binderType b

ffiArgType :: C.Arg -> MaybeT CG Ty
ffiArgType = \case
  C.StgLitArg l -> TySimple <$> lift (deriveNewName "t") <*> pure (getLitType l)
  C.StgVarArg b -> do
    (name, repType) <- lift $ genBinder b
    case C.binderType b of
      C.SingleValue C.UnliftedRep -> case C.binderTypeSig b of
        -- NOTE: byte array is allowed as FFI argument ; this is GHC special case
        "MutableByteArray# RealWorld" -> do
          n0 <- lift (deriveNewName "t")
          n1 <- lift (deriveNewName "t")
          pure $ TyCon n0 "MutableByteArray#" [TyCon n1 "RealWorld" []]

        "ByteArray#" -> do
          n0 <- lift (deriveNewName "t")
          pure $ TyCon n0 "ByteArray#" []

        "Weak# ThreadId" -> do
          n0 <- lift (deriveNewName "t")
          n1 <- lift (deriveNewName "t")
          pure $ TyCon n0 "Weak#" [TyCon n1 "ThreadId" []]

        "ThreadId#" -> do
          n0 <- lift (deriveNewName "t")
          pure $ TyCon n0 "ThreadId#" []

        _ -> fail ""

      C.SingleValue t -> do
        t1 <- MaybeT . pure $ getType (C.binderTypeSig b) t
        n0 <- lift (deriveNewName "t")
        pure $ TySimple n0 t1

      C.UnboxedTuple [C.VoidRep]
        | name `elem` ["GHC.Prim.coercionToken#", "GHC.Prim.realWorld#", "GHC.Prim.void#"]
        -> do
          n0 <- lift (deriveNewName "t")
          pure $ TyCon n0 name []

      _ -> fail ""

ffiRetType :: C.Type -> MaybeT CG Ty
ffiRetType = \case
  C.UnboxedTuple l -> mkFFIUTup l
  C.SingleValue r -> mkFFIUTup [r]
  t -> error $ "invalid FFI result value type: " ++ show t
  where
    mkFFIUTup l = do
      args <- forM (filter (/= C.VoidRep) l) $ \r ->
        TySimple <$> lift (deriveNewName "t") <*> MaybeT (pure $ getType "" r)
      lift $ mkUnboxedTuple args

mkUnboxedTuple :: [Ty] -> CG Ty
mkUnboxedTuple args = do
  n0 <- deriveNewName "t"
  pure $ case length args of
    0 -> TyCon n0 "GHC.Prim.(##)" []
    1 -> TyCon n0 "GHC.Prim.Unit#" args
    n -> TyCon n0 (packName $ "GHC.Prim.(#" ++ replicate (max 0 $ n-1) ',' ++ "#)") args

-- literal conversion

getLitPrimRep :: C.Lit -> PrimRep
getLitPrimRep = \case
  C.LitChar{}     -> Word64Rep
  C.LitString{}   -> AddrRep
  C.LitNullAddr   -> AddrRep
  C.LitFloat{}    -> FloatRep
  C.LitDouble{}   -> DoubleRep
  C.LitLabel{}    -> AddrRep
  C.LitNumber t _ -> case t of
    C.LitNumInteger -> error "impossible: LitNumInteger"
    C.LitNumNatural -> error "impossible: LitNumNatural"
    C.LitNumInt     -> Int64Rep
    C.LitNumInt64   -> Int64Rep
    C.LitNumWord    -> Word64Rep
    C.LitNumWord64  -> Word64Rep

getLitType :: C.Lit -> SimpleType
getLitType = \case
  C.LitChar{}     -> T_Char
  C.LitString{}   -> T_Addr
  C.LitNullAddr   -> T_Addr
  C.LitFloat{}    -> T_Float
  C.LitDouble{}   -> T_Double
  C.LitLabel{}    -> T_Addr
  C.LitNumber t _ -> case t of
    C.LitNumInteger -> error "impossible: LitNumInteger"
    C.LitNumNatural -> error "impossible: LitNumNatural"
    C.LitNumInt     -> T_Int64
    C.LitNumInt64   -> T_Int64
    C.LitNumWord    -> T_Word64
    C.LitNumWord64  -> T_Word64

convertLit :: C.Lit -> Lit
convertLit = \case
  C.LitNumber t i -> case t of
    C.LitNumInteger -> error "impossible: LitNumInteger"
    C.LitNumNatural -> error "impossible: LitNumNatural"
    C.LitNumInt     -> LInt64 $ fromIntegral i
    C.LitNumInt64   -> LInt64 $ fromIntegral i
    C.LitNumWord    -> LWord64 $ fromIntegral i
    C.LitNumWord64  -> LWord64 $ fromIntegral i
  C.LitFloat   f  -> LFloat $ realToFrac f
  C.LitDouble  f  -> LFloat $ realToFrac f
  C.LitString  s  -> LString s
  C.LitChar    c  -> LChar c
  C.LitNullAddr   -> LNullAddr
  C.LitLabel l s  -> case s of
    C.FunctionLabel i -> LCodeAddr l i
    C.DataLabel       -> LDataAddr l

-- data con and ty con conversion

convertTyCons :: [(C.ModuleName, [C.AlgTyCon])] -> [ConGroup]
convertTyCons tyConsGroups =
  [ mkConGroup mod tc
  | (mod, tyCons) <- tyConsGroups
  , tc <- tyCons
  , not (isUnboxed tc)
  ] where
      isUnboxed tc = case C.tcDataCons tc of
        [dc]  -> C.sdcRep dc == C.UnboxedDataCon
        _     -> False

mkConGroup :: C.ModuleName -> C.AlgTyCon -> ConGroup
mkConGroup mod tc
  = ConGroup
  { cgName  = packName $ BS8.unpack (C.getModuleName mod) ++ "." ++ BS8.unpack (C.tcName tc)
  , cgCons  = map (mkConSpec tc) $ C.tcDataCons tc
  }

mkConSpec :: C.AlgTyCon -> C.SDataCon -> ConSpec
mkConSpec tc C.SDataCon{..}
  = ConSpec
  { csName    = packName . BS8.unpack $ C.getModuleName sdcModule <> "." <> sdcName
  , csArgsRep = case sdcRep of
      C.AlgDataCon l    -> map getPrimRep l
      C.UnboxedDataCon  -> error $ "impossible - unboxed type: " ++ show tc
  }

-- stg ast conversion

emitLitArg :: RepType -> Lit -> CG Name
emitLitArg t l = do
  name <- deriveNewName "lit"
  emitCmd $ S (name, t, Lit l)
  pure name

visitArg :: C.Arg -> CG Name
visitArg a = fst <$> visitArgT a

visitArgT :: C.Arg -> CG (Name, RepType)
visitArgT = \case
  C.StgLitArg l -> do
    let t = SingleValue $ getLitPrimRep l
    (,t) <$> emitLitArg t (convertLit l)

  C.StgVarArg o -> do
    (name, repType) <- genBinder o
    n <- case name of
      "GHC.Prim.coercionToken#" -> emitLitArg (SingleValue VoidRep) $ LToken "GHC.Prim.coercionToken#"
      "GHC.Prim.realWorld#"     -> emitLitArg (SingleValue VoidRep) $ LToken "GHC.Prim.realWorld#"
      "GHC.Prim.void#"          -> emitLitArg (SingleValue VoidRep) $ LToken "GHC.Prim.void#"
      _ -> pure name
    pure (n, repType)


-- CG bind chain operations

type Binding = (Name, RepType, SimpleExp)
data Cmd
  = S Binding
  | L Binding
  | R [Binding]
  deriving Show

emitCmd :: Cmd -> CG ()
emitCmd c = modify' $ \env@Env{..} -> env {commands = c : commands}

openNewBindChain :: CG ()
openNewBindChain = modify' $ \env@Env{..} -> env {commands = [], commandStack = commands : commandStack}

closeBindChain :: CG (RepType, BindChain)
closeBindChain = do
  cmds <- gets commands
  modify' $ \env@Env{..} -> env {commands = head $ commandStack ++ [[]], commandStack = drop 1 commandStack}

  let mkBindChain :: Exp -> Cmd -> Exp
      mkBindChain e = \case
        S b -> LetS   [b] e
        L b -> Let    [b] e
        R l -> LetRec l   e

  pure $ case cmds of
    [S (_, t, var@Var{})] -> (t, var) -- HINT: already has Var terminator
    (S (name, t,_):_)     -> (t, foldl mkBindChain (Var name) cmds)
    _                     -> error $ "invalid bind chain: " ++ show (reverse cmds)

genResultName :: Maybe Name -> CG Name
genResultName = \case
  Just n  -> pure n
  Nothing -> deriveNewName "result"

-- primop conversion
-- GHC/Stg Prim Op call conversion

primMap :: Map Name External
primMap = Map.fromList [(eName, e) | e@External{..} <- prims] where
  Program prims _ _ _  = GHCPrim.primPrelude

visitOpApp :: Name -> C.StgOp -> [C.Arg] -> C.Type -> CG ()
visitOpApp resultName op args ty = do
  ffiTys <- runMaybeT $ do
    argsTy <- mapM ffiArgType args
    retTy <- ffiRetType ty
    pure (retTy, argsTy)
  let resultRepType = convertType ty
  case op of
    C.StgPrimOp prim -> do
      let name = packName (BS8.unpack prim)
      case Map.lookup name primMap of
        Nothing -> do
          let errLit = Lit $ LError $ "Unsupported GHC primop: " <> BS8.pack (BS8.unpack prim ++ " return type: " ++ show ty)
          emitCmd $ S (resultName, resultRepType, errLit)

        Just e  -> do
          addExternal e
          args2 <- mapM visitArg args
          emitCmd $ S (resultName, resultRepType, App name args2)

    C.StgPrimCallOp _ -> do
      let errLit = Lit $ LError "GHC PrimCallOp is not supported"
      emitCmd $ S (resultName, resultRepType, errLit)

    C.StgFCallOp f@C.ForeignCall{..} -> case foreignCTarget of
      C.DynamicTarget -> do
        let (fnTy:argsTy) = map showArgType args
            retTy         = show ty
            errLit        =  Lit . LError . BS8.pack $ "DynamicTarget is not supported: (" ++ fnTy ++ ") :: " ++ intercalate " -> " (argsTy ++ [retTy])
        emitCmd $ S (resultName, resultRepType, errLit)

      C.StaticTarget labelName -> case ffiTys of
        Just (retTy, argsTy) -> do
          let name = packName $ BS8.unpack labelName
          addExternal External
            { eName       = name
            , eRetType    = retTy
            , eArgsType   = argsTy
            , eEffectful  = True
            , eKind       = FFI
            }
          args2 <- mapM visitArg args
          emitCmd $ S (resultName, resultRepType, App name args2)

        _ -> do
          let name    = BS8.unpack labelName
              argsTy  = map showArgType args
              retTy   = show ty
              errLit  = Lit . LError . BS8.pack $ "Unsupported foreign function type: " ++ name ++ " :: " ++ intercalate " -> " (argsTy ++ [retTy])
          emitCmd $ S (resultName, resultRepType, errLit)

{-
  always generate result var for expr simple exps
  the redundant result = var operation will be removed in closeBindChain operation
-}
-- this builds a bind chain
visitExpr :: Maybe Name -> C.Expr -> CG ()
visitExpr mname expr = case expr of
  -- S item
  -- generate result var if necessary
  C.StgLit lit -> do
    name <- genResultName mname
    emitCmd $ S (name, SingleValue $ getLitPrimRep lit, Lit $ convertLit lit)

  -- S item
  C.StgApp var [] _ _ -> do
    (n, t) <- genBinder var
    name <- genResultName mname
    case t of
      SingleValue LiftedRep -> do
        -- NOTE: force thunk
        emitCmd $ S (name, SingleValue UnliftedRep, App n [])
      _ -> do
        emitCmd $ S (name, t, Var n)

  -- S item
  -- generate result var if necessary
  C.StgApp fun args t _ -> do
    fun2 <- genName fun
    args2 <- mapM visitArg args
    name <- genResultName mname
    emitCmd $ S (name, convertType t, App fun2 args2)

  -- S item
  -- generate result var if necessary
  C.StgConApp con args _ -> do
    (args2, argsTy) <- unzip <$> mapM visitArgT args
    let con2 = genDataConName con
        t = if isUnboxedTuple (BS8.pack $ unpackName con2) then UnboxedTuple (map utItemTy argsTy) else SingleValue UnliftedRep
        utItemTy (SingleValue i) = i
        utItemTy i = error $ "invalid unboxed tuple argument type: " ++ show i
    name <- genResultName mname
    emitCmd $ S (name, t, Con con2 args2)

  -- S item
  -- generate result var if necessary
  C.StgOpApp op args ty -> do
    name <- genResultName mname
    visitOpApp name op args ty

  C.StgCase scrutExpr scrutResult alts  -> do
    -- collect scrutinee Cmds
    -- caseses
      -- pattern match: emit case an create alts ; generate result var if necessary
      -- eval: continue building the binding chain (default rhs) ; no need for result var because binding chain continues
    scrutName <- genName scrutResult
    visitExpr (Just scrutName) scrutExpr
    case alts of
      -- NOTE: force convention in STG
      [C.Alt C.AltDefault [] rhsExpr] -> visitExpr mname rhsExpr

      -- normal case
      _ -> do
        (altResultRepTypes, alts2) <- unzip <$> mapM visitAlt alts
        name <- genResultName mname
        emitCmd $ S (name, joinRepTypes ("case scrut: " ++ show (P.pretty scrutResult)) altResultRepTypes, Case scrutName alts2)

  ---------------------------
  -- IMPORTANT: let binder is a Con or Closure, so it does not affect the current bind chain!!!!!
  ---------------------------
  -- L item ; no need for result var because binding chain continues
  C.StgLet (C.StgNonRec b r) e -> do
    name <- genName b
    (t, exp) <- visitRhs r
    emitCmd $ L (name, t, exp)
    visitExpr mname e

  -- R item ; no need for result var because binding chain continues
  C.StgLet (C.StgRec bs) e -> do
    bs2 <- forM bs $ \(b, r) -> do
      name <- genName b
      (t, exp) <- visitRhs r
      pure (name, t, exp)
    emitCmd $ R bs2
    visitExpr mname e

  -- L item ; no need for result var because binding chain continues
  C.StgLetNoEscape (C.StgNonRec b r) e -> do
    name <- genName b
    (t, exp) <- visitRhs r
    emitCmd $ L (name, t, exp)
    visitExpr mname e

  -- R item ; no need for result var because binding chain continues
  C.StgLetNoEscape (C.StgRec bs) e -> do
    bs2 <- forM bs $ \(b, r) -> do
      name <- genName b
      (t, exp) <- visitRhs r
      pure (name, t, exp)
    emitCmd $ R bs2
    visitExpr mname e

  _ -> error . printf "unsupported expr %s" $ show expr

visitAlt :: C.Alt -> CG (RepType, Alt)
visitAlt (C.Alt altCon argIds body) = do
  openNewBindChain
  cpat <- case altCon of
    C.AltDataCon dc  -> NodePat (genDataConName dc) <$> mapM genName argIds
    C.AltLit lit     -> pure . LitPat $ convertLit lit
    C.AltDefault     -> pure DefaultPat
  -- bind chain
  visitExpr Nothing body
  (rt, body2) <- closeBindChain
  altName <- deriveNewName "alt"
  pure (rt, Alt altName cpat body2)

joinRepTypes :: String -> [RepType] -> RepType
joinRepTypes msg = foldr1 f where
  f (SingleValue LiftedRep) (SingleValue UnliftedRep) = SingleValue LiftedRep
  f (SingleValue UnliftedRep) (SingleValue LiftedRep) = SingleValue LiftedRep
  f (SingleValue a) (UnboxedTuple [b])                = f (SingleValue a) (SingleValue b) -- Q: why do we need this?
  f (UnboxedTuple [a]) (SingleValue b)                = f (SingleValue a) (SingleValue b) -- Q: why do we need this?
  f (UnboxedTuple []) (UnboxedTuple [VoidRep])        = UnboxedTuple []
  f (UnboxedTuple [VoidRep]) (UnboxedTuple [])        = UnboxedTuple []
{-
  f (UnboxedTuple a) (UnboxedTuple b)
    | fa <- filter (/=VoidRep) a
    , fb <- filter (/=VoidRep) b
    , fa == fb
    = UnboxedTuple fa
-}
  f a b
    | a == b    = a
    | otherwise = error $ "can not join RepType: " ++ show (a, b) ++ "\n" ++ msg

visitRhs :: C.Rhs -> CG (RepType, SimpleExp)
visitRhs = \case
  C.StgRhsCon con args -> do
    c <- Con (genDataConName con) <$> mapM visitArg args
    pure (SingleValue UnliftedRep, c)

  C.StgRhsClosure _ bs body -> do
    openNewBindChain
    bs2 <- mapM genBinder bs
    visitExpr Nothing body
    (_, body2) <- closeBindChain
    pure (SingleValue LiftedRep, Closure [] bs2 body2)

visitTopRhs :: C.Binder -> C.Rhs -> CG ()
visitTopRhs b = \case
  C.StgRhsClosure _ bs body -> do
    openNewBindChain
    name <- genName b
    params <- mapM genBinder bs
    visitExpr Nothing body
    (_, body2) <- closeBindChain
    addDef (Def name params body2)

  C.StgRhsCon con args -> do
    openNewBindChain
    name <- genName b
    resultVar <- deriveNewName "result"
    con2 <- Con (genDataConName con) <$> mapM visitArg args
    emitCmd $ S (resultVar, SingleValue LiftedRep, con2)
    (_, body) <- closeBindChain
    addDef $ Def name [] body

visitTopBinder :: C.TopBinding -> CG ()
visitTopBinder = \case
  C.StgTopStringLit b s -> do
    name <- genName b
    addStaticData $ StaticData name (StaticString s)

  C.StgTopLifted (C.StgNonRec b r) -> do
    visitTopRhs b r

  C.StgTopLifted (C.StgRec bs) -> do
    mapM_ (uncurry visitTopRhs) bs

registerTopBinderName :: C.TopBinding -> CG ()
registerTopBinderName = \case
  C.StgTopStringLit b _ -> do
    genName b
    pure ()

  C.StgTopLifted (C.StgNonRec b _) -> do
    genName b
    pure ()

  C.StgTopLifted (C.StgRec bs) -> forM_ bs $ \(b, _) -> do
    genName b
    pure ()

{-
  rewrite and do on the fly:
    done - lit arg to name
    done - var terminator
    done - rep type on binding site
    done - convert directly to Lambda2

  idea:
    walk ext stg and collect values and expressions in state monad
-}

visitModule :: C.Module -> CG ()
visitModule C.Module{..} = do
  mapM_ registerTopBinderName moduleTopBindings
  mapM_ visitTopBinder moduleTopBindings

codegenLambda :: C.Module -> IO Program
codegenLambda mod = do
  let modName   = packName . BS8.unpack . C.getModuleName $ C.moduleName mod
  Env{..} <- execStateT (visitModule mod) (Env modName mempty mempty mempty mempty mempty mempty mempty mempty)
  let conGroups = convertTyCons $ C.moduleAlgTyCons mod
  pure . smashLet $ Program (Map.elems externals) conGroups staticData defs
