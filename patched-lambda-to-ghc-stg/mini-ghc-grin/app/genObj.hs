{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad

import System.Environment

import StgLoopback

import Stg.Util
import Stg.ToStg

import qualified GHC.Driver.Types as GHC


{-
  = StgModule
  { stgUnitId       :: UnitId
  , stgModuleName   :: ModuleName
  , stgModuleTyCons :: [TyCon]
  , stgTopBindings  :: [StgTopBinding]
  , stgForeignStubs :: ForeignStubs
  , stgForeignFiles :: [(ForeignSrcLang, FilePath)]
  }
-}

main :: IO ()
main = do
  let cg = NCG

  stgbins <- getArgs
  forM_ stgbins $ \stgbinName -> do
    putStrLn $ "reading   " ++ stgbinName
    extStgModule <- readStgbin stgbinName
    let StgModule{..} = toStg extStgModule
        oName         = stgbinName ++ ".o"
    putStrLn $ "compiling " ++ oName
    putStrLn $ unlines $ map show stgIdUniqueMap
    compileToObject cg stgUnitId stgModuleName {-stgForeignStubs-}GHC.NoStubs stgModuleTyCons stgTopBindings oName
    -- TODO: simplify API to: compileToObject cg stgModule oName
