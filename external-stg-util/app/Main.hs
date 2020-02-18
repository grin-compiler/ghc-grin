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

import Text.Regex.TDFA
import Text.Regex.TDFA.Common (Regex)
import Text.Regex.TDFA.Text

import Stg.Syntax
import Stg.Pretty
import Stg.Util


filterBindings :: Regex -> Module -> Module
filterBindings re m =
    m { moduleTopBindings = mapMaybe filterTopBinding $ moduleTopBindings m }
  where
    filterTopBinding bn = case bn of
      StgTopStringLit b _           | nameMatches b   -> Just bn
      StgTopLifted (StgNonRec b _)  | nameMatches b   -> Just bn
      StgTopLifted (StgRec bs)      | not $ null bs'  -> Just . StgTopLifted . StgRec $ bs'
        where bs' = filter (\(b,_) -> nameMatches b) bs
      _                                               -> Nothing

    nameMatches :: Binder -> Bool
    nameMatches b = matchTest re (binderUniqueName b)

modes :: Parser (IO ())
modes = subparser
    (  mode "show" showMode (progDesc "print Stg")
    <> mode "show-prep-core" showPCoreMode (progDesc "print prep Core")
    <> mode "show-core" showCoreMode (progDesc "print Core")
    )
  where
    mode :: String -> Parser a -> InfoMod a -> Mod CommandFields a
    mode name f opts = command name (info (helper <*> f) opts)

    dumpFile :: Parser FilePath
    dumpFile = argument str (metavar "DUMP FILE" <> help "CBOR dump file")

    filterCond :: Parser (Module -> Module)
    filterCond =
        fmap (maybe id filterBindings)
        $ option (str >>= fmap Just . makeRegexM')
                 (short 'f' <> long "filter" <> value Nothing <> help "filter bindings by name")
      where
        makeRegexM' = makeRegexM :: String -> ReadM Regex

    showMode :: Parser (IO ())
    showMode =
        run <$> filterCond <*> dumpFile
      where
        run filterFn fname = do
            dump <- filterFn <$> Stg.Util.readDump fname
            print $ pprModule dump

    showPCoreMode :: Parser (IO ())
    showPCoreMode =
        run <$> dumpFile
      where
        run fname = do
          dump <- Stg.Util.readDump fname
          putStrLn . BS8.unpack . modulePrepCoreSrc $ dump

    showCoreMode :: Parser (IO ())
    showCoreMode =
        run <$> dumpFile
      where
        run fname = do
          dump <- Stg.Util.readDump fname
          putStrLn . BS8.unpack . moduleCoreSrc $ dump

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
