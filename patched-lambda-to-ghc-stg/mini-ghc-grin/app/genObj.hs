{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import System.Environment

import StgLoopback

import Stg.Util
import Stg.ToStg

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
main = runGhc (Just libdir) $ liftIO $ do
  let cg = NCG

  stgbins <- getArgs
  forM_ stgbins $ \stgbinName -> do
    putStrLn $ "reading   " ++ stgbinName
    extStgModule <- readStgbin stgbinName
    let StgModule{..} = toStg extStgModule
        oName         = stgbinName ++ ".o"
    putStrLn $ "compiling " ++ oName
    putStrLn $ unlines $ map show stgIdUniqueMap

    -- HINT: the stubs are compiled at link time
    compileToObject cg stgUnitId stgModuleName GHC.NoStubs stgModuleTyCons stgTopBindings oName

    -- TODO: simplify API to: compileToObject cg stgModule oName
