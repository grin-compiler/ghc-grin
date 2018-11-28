{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Lambda.GhcPrimOpsBoxed where

import qualified Data.Set as Set
import Grin.TH

ghcPrimOps = [prog|

  grinMain =
    result_main <- Main.main $ #False
    case result_main of
      (C"GHC.Prim.Unit#" ptr) ->
        _prim_int_print $ 0
      #default ->
        _prim_int_print $ -1

  "_ghc__stg_foreign_call {PlaySafe CCallConv StaticTarget \"_prim_int_print\"}{(# State# RealWorld #)}" a b =
    (Cint64 i) <- fetch a -- Q: should this be eval?
    _prim_int_print i
    pure (C"GHC.Prim.(##)")

  "_ghc_negateInt#{Int#}" a =
    (Cint64 i) <- fetch a
    c <- _prim_int_mul -1 i
    pure (Cint64 c)

  "_ghc_*#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    c <- _prim_int_mul i j
    pure (Cint64 c)

  "_ghc_+#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    c <- _prim_int_add i j
    pure (Cint64 c)

  "_ghc_-#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    c <- _prim_int_sub i j
    pure (Cint64 c)

  "_ghc_<#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    r <- _prim_int_lt i j
    _ghc_bool_to_int r

  "_ghc_>=#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    r <- _prim_int_ge i j
    _ghc_bool_to_int r

  "_ghc_/=#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    r <- _prim_int_ne i j
    _ghc_bool_to_int r

  "_ghc_<=#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    r <- _prim_int_le i j
    _ghc_bool_to_int r

  "_ghc_==#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    r <- _prim_int_eq i j
    _ghc_bool_to_int r

  "_ghc_>#{Int#}" a b =
    (Cint64 i) <- fetch a
    (Cint64 j) <- fetch b
    r <- _prim_int_gt i j
    _ghc_bool_to_int r

  _ghc_bool_to_int a =
    case a of
      #False  -> pure 0
      #True   -> pure 1

  "_ghc_tagToEnum#{Bool}" a =
    (Cint64 i) <- fetch a
    case i of
      0 -> pure (CGHC.Types.False)
      1 -> pure (CGHC.Types.True)

  "_ghc_indexIntArray#{Int#}" array index = fetch index -- TODO

|]
