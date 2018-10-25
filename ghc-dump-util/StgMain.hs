{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List (sortBy)
import Data.Monoid
import Data.Ord

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Text.Regex.TDFA
import Text.Regex.TDFA.Common (Regex)
import Text.Regex.TDFA.Text

--import GhcDump_Ast (Binder(..), binderUniqueName)
--import GhcDump.Pretty (PrettyOpts(..))
import GhcDump.StgPretty
import GhcDump.StgUtil
import GhcDump_StgAst


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
     $ mode "show" showMode (progDesc "print Stg")
  where
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

    prettyOpts :: Parser PrettyOpts
    prettyOpts =
        PrettyOpts
          <$> switch (short 'u' <> long "show-uniques" <> help "Show binder uniques")
          <*> switch (short 'i' <> long "show-idinfo" <> help "Show IdInfo of bindings")
          <*> switch (short 'T' <> long "show-let-types" <> help "Show type signatures for let-bound binders")
          <*> switch (short 'U' <> long "show-unfoldings" <> help "Show unfolding templates")

    showMode =
        run <$> filterCond <*> prettyOpts <*> dumpFile
      where
        run filterFn opts fname = do
            dump <- filterFn <$> GhcDump.StgUtil.readDump fname
            print $ pprModule opts dump

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
