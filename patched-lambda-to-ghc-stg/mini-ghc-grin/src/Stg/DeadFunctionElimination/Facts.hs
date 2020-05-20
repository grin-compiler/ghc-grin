{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, OverloadedStrings #-}
module Stg.DeadFunctionElimination.Facts where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import Control.Monad

import System.IO
import System.FilePath

import Stg.Syntax

writeDfeFacts :: FilePath -> Module -> IO ()
writeDfeFacts stgbinFname Module{..} = do
  let
      openFact :: String -> IO Handle
      openFact name = do
        let factFile = stgbinFname -<.> name
        h <- openFile factFile WriteMode
        putStrLn factFile
        pure h

      closeFact :: Handle -> IO ()
      closeFact h = do
        hFlush h
        hClose h

      getTopBinders :: TopBinding -> [Binder]
      getTopBinders = \case
        StgTopLifted (StgNonRec b _)  -> [b]
        StgTopLifted (StgRec bs)      ->  map fst bs
        StgTopStringLit b _           -> [b]

      topBinders :: [Binder]
      topBinders = concatMap getTopBinders moduleTopBindings

      extBinders :: [Binder]
      extBinders = concatMap snd . concatMap snd $ moduleExternalTopIds

  -- from module

  -- export LiveSource
  factLiveSource <- openFact "LiveSource.facts"
  forM_ topBinders $ \b -> when (binderScope b == ForeignExported) $ do
    BS8.hPutStrLn factLiveSource $ binderUniqueName b
  closeFact factLiveSource

  -- export TyCon with DataCons
  factTyCon <- openFact "TyCon.facts"
  forM_ (concatMap snd . concatMap snd $ moduleTyCons) $ \tc -> do
    let tcName = tyConUniqueName tc
    forM_ (tcDataCons tc) $ \dc -> do
      BS8.hPutStrLn factTyCon $ tcName <> "\t" <> dataConUniqueName dc
  closeFact factTyCon

  -- from AST
  factTyConReference    <- openFact "TyConReference.facts"
  factDataConReference  <- openFact "DataConReference.facts"
  factFunReference      <- openFact "FunReference.facts"

  let
      funNameMap :: Map BinderId Name
      funNameMap = Map.fromList [(binderId b, binderUniqueName b) | b <- topBinders ++ extBinders]

      addFunRef :: Name -> Binder -> IO ()
      addFunRef fun b = case Map.lookup (binderId b) funNameMap of
        Nothing -> pure ()
        Just n  -> BS8.hPutStrLn factFunReference $ fun <> "\t" <> n

      addDataConRef :: Name -> DataCon -> IO ()
      addDataConRef fun dc = do
        BS8.hPutStrLn factDataConReference $ fun <> "\t" <> dataConUniqueName dc

      addTyConRef :: Name -> TyCon -> IO ()
      addTyConRef fun tc = do
        BS8.hPutStrLn factTyConReference $ fun <> "\t" <> tyConUniqueName tc

  let
      visitTopBinding :: TopBinding -> IO ()
      visitTopBinding = \case
        StgTopLifted (StgNonRec b e)  -> visitRhs (binderUniqueName b) e
        StgTopLifted (StgRec bs)      -> sequence_ [visitRhs (binderUniqueName b) e | (b, e) <- bs]
        StgTopStringLit{}             -> pure ()

      visitBinding :: Name -> Binding -> IO ()
      visitBinding fun = \case
        StgNonRec _ rhs -> visitRhs fun rhs
        StgRec    l     -> mapM_ (visitRhs fun) $ map snd l

      visitRhs :: Name -> Rhs -> IO ()
      visitRhs fun = \case
        StgRhsClosure _ args expr -> mapM_ (addFunRef fun) args >> visitExpr fun expr
        StgRhsCon con args        -> addDataConRef fun con >> mapM_ (visitArg fun) args

      visitArg :: Name -> Arg -> IO ()
      visitArg fun = \case
        StgVarArg v               -> addFunRef fun v
        StgLitArg{}               -> pure ()

      visitExpr :: Name -> Expr -> IO ()
      visitExpr fun = \case
        StgApp f args _ _         -> addFunRef fun f >> mapM_ (visitArg fun) args
        StgLit{}                  -> pure ()
        StgConApp con args _      -> addDataConRef fun con >> mapM_ (visitArg fun) args

        -- NOTE: _op could only refer to function that are implemented in the low level IRs but not in STG
        StgOpApp _op args _ mtc   -> mapM_ (visitArg fun) args >> case mtc of
                                      Nothing -> pure ()
                                      Just tc -> addTyConRef fun tc

        StgCase expr _ aty alts   -> visitExpr fun expr >> visitAltType fun aty >> mapM_ (visitAlt fun) alts
        StgLet b  e               -> visitBinding fun b >> visitExpr fun e
        StgLetNoEscape b  e       -> visitBinding fun b >> visitExpr fun e

      visitAltType :: Name -> AltType -> IO ()
      visitAltType fun = \case
        PolyAlt       -> pure ()
        MultiValAlt{} -> pure ()
        PrimAlt{}     -> pure ()
        AlgAlt tc     -> addTyConRef fun tc

      visitAlt :: Name -> Alt -> IO ()
      visitAlt fun (Alt ac _ expr) = visitAltCon fun ac >> visitExpr fun expr

      visitAltCon :: Name -> AltCon -> IO ()
      visitAltCon fun = \case
        AltDataCon dc -> addDataConRef fun dc
        AltLit{}      -> pure ()
        AltDefault    -> pure ()

  -- traverse AST and collect facts
  mapM_ visitTopBinding moduleTopBindings

  -- close facts
  closeFact factTyConReference
  closeFact factDataConReference
  closeFact factFunReference
