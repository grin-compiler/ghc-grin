{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module PP (tuple) where

tuple :: [String] -> String
tuple []     = "()"
tuple [a]    = "(" ++ a ++ ")"
tuple (a:as) = "(" ++ a ++ concat (map ((++) ", ") as) ++ ")"
