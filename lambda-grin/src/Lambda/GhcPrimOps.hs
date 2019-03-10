{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Lambda.GhcPrimOps where

import qualified Data.Set as Set
import Grin.TH

isUnboxedPrim name = Set.member name unboxedOps where
  unboxedOps = Set.fromList
    [ "_ghc_negateInt#{Int#}"
    , "_ghc_*#{Int#}"
    , "_ghc_+#{Int#}"
    , "_ghc_-#{Int#}"
    , "_ghc_<#{Int#}"
    , "_ghc_>=#{Int#}"
    , "_ghc_/=#{Int#}"
    , "_ghc_<=#{Int#}"
    , "_ghc_==#{Int#}"
    , "_ghc_>#{Int#}"
    , "_ghc_indexIntArray#{Int#}"
    ]

ghcPrimOps = [prog|

  grinMain =
    result_main <- ":Main.main" $ #"GHC.Prim.realWorld#"
    case result_main of
      (C"GHC.Prim.Unit#" ptr) ->
        _prim_int_print $ 0
      #default ->
        _prim_int_print $ -1
|]
