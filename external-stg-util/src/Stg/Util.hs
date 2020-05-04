module Stg.Util
    ( -- * Convenient IO
      readStgbin, readStgbin',
      readStgbinInfo
    ) where

import Prelude hiding (readFile)

import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get

import Stg.Syntax
import Stg.Reconstruct

readStgbin' :: FilePath -> IO SModule
readStgbin' fname = decode <$> BSL.readFile fname

readStgbin :: FilePath -> IO Module
readStgbin fname = reconModule <$> readStgbin' fname

readStgbinInfo :: FilePath -> IO (Name, UnitId, ModuleName, ForeignStubs, Bool, [(UnitId, [ModuleName])])
readStgbinInfo fname = decode <$> BSL.readFile fname
