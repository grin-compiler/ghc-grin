{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List (sortBy)
import Data.Monoid
import Data.Ord

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.ByteString.Char8 as BS8

import Stg.Syntax
import Stg.Pretty
import Stg.Util


modes :: Parser (IO ())
modes = subparser
    (  mode "show" showMode (progDesc "print Stg")
    <> mode "show-prep-core" showPCoreMode (progDesc "print prep Core")
    <> mode "show-core" showCoreMode (progDesc "print Core")
    <> mode "show-ghc-stg" showGHCStgMode (progDesc "print GHC Stg")
    )
  where
    mode :: String -> Parser a -> InfoMod a -> Mod CommandFields a
    mode name f opts = command name (info (helper <*> f) opts)

    dumpFile :: Parser FilePath
    dumpFile = argument str (metavar "DUMP FILE" <> help "STGBIN dump file")

    showMode :: Parser (IO ())
    showMode =
        run <$> dumpFile
      where
        run fname = do
            dump <- Stg.Util.readStgbin fname
            print $ pprModule dump

    showPCoreMode :: Parser (IO ())
    showPCoreMode =
        run <$> dumpFile
      where
        run fname = do
          dump <- Stg.Util.readStgbin fname
          putStrLn . BS8.unpack . modulePrepCoreSrc $ dump

    showCoreMode :: Parser (IO ())
    showCoreMode =
        run <$> dumpFile
      where
        run fname = do
          dump <- Stg.Util.readStgbin fname
          putStrLn . BS8.unpack . moduleCoreSrc $ dump

    showGHCStgMode :: Parser (IO ())
    showGHCStgMode =
        run <$> dumpFile
      where
        run fname = do
          dump <- Stg.Util.readStgbin fname
          putStrLn . BS8.unpack . moduleStgSrc $ dump

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
