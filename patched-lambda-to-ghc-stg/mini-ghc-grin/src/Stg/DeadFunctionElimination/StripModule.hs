{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, OverloadedStrings #-}
module Stg.DeadFunctionElimination.StripModule where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8

import System.FilePath
import System.Directory

import Stg.Syntax

stripDeadParts :: FilePath -> Module -> IO Module
stripDeadParts stgappName mod = do
  let liveFunFname = {-stgappName -<.> -}"LiveFunName.csv"
      liveDataConFname = {-stgappName -<.> -}"LiveDataConName.csv"
  liveFunSet0 <- Set.fromList . BS8.lines <$> BS8.readFile liveFunFname
  liveDataConSet <- Set.fromList . BS8.lines <$> BS8.readFile liveDataConFname
  let liveFunSet = Set.union liveFunSet0 liveDataConSet

  putStrLn "stripDeadParts"

  let dropDeadBinding :: TopBinding -> Maybe TopBinding
      dropDeadBinding tb = case tb of
        StgTopLifted (StgNonRec b _)
          | Set.member (binderUniqueName b) liveFunSet -> Just tb
          | otherwise -> Nothing

        StgTopLifted (StgRec bs)
          | lives <- [a | a@(b,_) <- bs, Set.member (binderUniqueName b) liveFunSet]
          , not $ null lives
          -> Just $ StgTopLifted (StgRec lives)
          | otherwise -> Nothing

        StgTopStringLit b l
          | Set.member (binderUniqueName b) liveFunSet -> Just tb
          | otherwise -> Nothing

  -- TODO: strip stgModuleTyCons
  pure mod {moduleTopBindings = catMaybes $ map dropDeadBinding $ moduleTopBindings mod}

tryStripDeadParts :: FilePath -> Module -> IO Module
tryStripDeadParts stgappName mod = do
  let liveFunFname = {-stgappName -<.> -}"LiveFunName.csv"
  hasLivenessInfo <- doesFileExist liveFunFname
  if hasLivenessInfo
    then stripDeadParts stgappName mod
    else pure mod
