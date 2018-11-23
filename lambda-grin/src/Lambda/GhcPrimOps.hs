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
    result_main <- Main.main $ #False
    case result_main of
      (C"GHC.Prim.Unit#" ptr) ->
        _prim_int_print $ 0
      #default ->
        _prim_int_print $ -1

  "_ghc__stg_foreign_call {PlaySafe CCallConv StaticTarget \"_prim_int_print\"}{(# State# RealWorld #)}" i b =
    _prim_int_print i
    pure (C"GHC.Prim.(##)")

  "_ghc_negateInt#{Int#}" a =
    _prim_int_mul -1 a

  "_ghc_*#{Int#}" a b =
    _prim_int_mul a b

  "_ghc_+#{Int#}" a b =
    _prim_int_add a b

  "_ghc_-#{Int#}" a b =
    _prim_int_sub a b

  "_ghc_<#{Int#}" a b =
    r <- _prim_int_lt a b
    _ghc_bool_to_int r

  "_ghc_>=#{Int#}" a b =
    r <- _prim_int_ge a b
    _ghc_bool_to_int r

  "_ghc_/=#{Int#}" a b =
    r <- _prim_int_ne a b
    _ghc_bool_to_int r

  "_ghc_<=#{Int#}" a b =
    r <- _prim_int_le a b
    _ghc_bool_to_int r

  "_ghc_==#{Int#}" a b =
    r <- _prim_int_eq a b
    _ghc_bool_to_int r

  "_ghc_>#{Int#}" a b =
    r <- _prim_int_gt a b
    _ghc_bool_to_int r

  _ghc_bool_to_int a =
    case a of
      #False  -> pure 0
      #True   -> pure 1

  "_ghc_tagToEnum#{Bool}" a =
    case a of
      0 -> pure (CGHC.Types.False)
      1 -> pure (CGHC.Types.True)

  "_ghc_indexIntArray#{Int#}" array index = pure 0 -- TODO

|]
