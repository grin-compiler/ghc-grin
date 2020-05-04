{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class

import System.Environment

import Stg.Util
import Stg.ToStg

import qualified GHC.Stg.Syntax     as GHC
import qualified Outputable         as GHC
import qualified GHC.Driver.Session as GHC

import GHC.Paths ( libdir )
import GHC

showSDoc :: GHC.SDoc -> String
showSDoc = GHC.showSDoc GHC.unsafeGlobalDynFlags

main :: IO ()
main = runGhc (Just libdir) . liftIO $ do

  stgbins <- getArgs
  forM_ stgbins $ \stgbinName -> do
    putStrLn $ "reading   " ++ stgbinName
    extStgModule <- readStgbin stgbinName
    let StgModule{..} = toStg extStgModule
    putStrLn . showSDoc $ GHC.pprStgTopBindings stgTopBindings
