{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import System.Environment

import StgLoopback

import Stg.Util
import Stg.ToStg
import Stg.DeadFunctionElimination.StripModule

import qualified GHC.Driver.Types as GHC

import GHC
import GHC.Paths ( libdir )

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
main = runGhc (Just libdir) $ do
  let cg = NCG

  stgbins <- liftIO getArgs
  forM_ stgbins $ \stgbinName -> do
    extStgModule <- liftIO $ do
      putStrLn $ stgbinName
      readStgbin stgbinName

    strippedExtModule <- liftIO $ tryStripDeadParts {-stgbinName-}"." extStgModule -- TODO: fix liveness input name

    let StgModule{..} = toStg strippedExtModule
        oName         = stgbinName ++ ".o"
    --liftIO $ putStrLn $ "compiling " ++ oName
    --putStrLn $ unlines $ map show stgIdUniqueMap

    -- HINT: the stubs are compiled at link time
    compileToObjectM cg stgUnitId stgModuleName GHC.NoStubs stgModuleTyCons stgTopBindings oName

    -- TODO: simplify API to: compileToObject cg stgModule oName
