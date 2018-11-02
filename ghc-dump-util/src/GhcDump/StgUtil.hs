module GhcDump.StgUtil
    ( -- * Convenient IO
      readDump, readDump',
      readDumpInfo
    ) where

import Prelude hiding (readFile)

import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get

import GhcDump_StgAst
import GhcDump.StgReconstruct

readDump' :: FilePath -> IO SModule
readDump' fname = decode <$> BSL.readFile fname

readDump :: FilePath -> IO Module
readDump fname = reconModule <$> readDump' fname

readDumpInfo :: FilePath -> IO (ModuleName, [ModuleName])
readDumpInfo fname = decode <$> BSL.readFile fname
