{-# LANGUAGE TemplateHaskell #-}
module Lambda.TH
  ( progConst
  ) where

import Data.List (sort)
import Data.Char
import Data.Data
import Data.Maybe
import Text.Megaparsec

import qualified Lambda.Parse as P
import qualified Data.Text as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)

-- NOTE: does not support metavariables
progConst :: QuasiQuoter
progConst = QuasiQuoter
  { quoteExp = \input -> do
      let src = T.pack $ normalizeQQInput input
      case P.parseLambda "" src of
        Left  e -> fail $ parseErrorPretty' src e
        Right p -> liftDataWithText p
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

--
-- NOTE: copy-paste utility from NeatInterpolation.String hidden module
--
normalizeQQInput :: [Char] -> [Char]
normalizeQQInput = trim . unindent' . tabsToSpaces
  where
    unindent' :: [Char] -> [Char]
    unindent' s =
      case lines s of
        head:tail ->
          let
            unindentedHead = dropWhile (== ' ') head
            minimumTailIndent = minimumIndent . unlines $ tail
            unindentedTail = case minimumTailIndent of
              Just indent -> map (drop indent) tail
              Nothing -> tail
          in unlines $ unindentedHead : unindentedTail
        [] -> []

trim :: [Char] -> [Char]
trim = dropWhileRev isSpace . dropWhile isSpace

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

unindent :: [Char] -> [Char]
unindent s =
  case minimumIndent s of
    Just indent -> unlines . map (drop indent) . lines $ s
    Nothing -> s

tabsToSpaces :: [Char] -> [Char]
tabsToSpaces ('\t':tail) = "    " ++ tabsToSpaces tail
tabsToSpaces (head:tail) = head : tabsToSpaces tail
tabsToSpaces [] = []

minimumIndent :: [Char] -> Maybe Int
minimumIndent =
  listToMaybe . sort . map lineIndent
    . filter (not . null . dropWhile isSpace) . lines

-- | Amount of preceding spaces on first line
lineIndent :: [Char] -> Int
lineIndent = length . takeWhile (== ' ')
