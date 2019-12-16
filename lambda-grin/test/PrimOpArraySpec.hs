{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpArraySpec where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.IORef
import Test.Hspec
import Test.QuickCheck
import System.IO
import Text.Show.Pretty (pPrint, ppShow)
import Text.PrettyPrint.ANSI.Leijen

import Lambda.TH
import Lambda.ControlFlowAnalysisM
import Grin.Pretty (PP(..))

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  ----------------------------
  usedRules <- runIO $ newIORef (Set.empty :: Set.Set [Text.Text])

  let filterAndSort keys m = fmap sort $ Map.restrictKeys m (Set.fromList keys)

      sameAs :: Show a => a -> a -> IO ()
      sameAs a b = (PP (ppShow a)) `shouldBe` (PP (ppShow b))

      toArrOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "Array"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------
{-
  primop effectful
    + "newArray#" :: T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}

  primop pure
    - "sameMutableArray#" :: {"MutableArray#" %s %a} -> {"MutableArray#" %s %a} -> T_Int64

  primop effectful
    + "readArray#"  :: {"MutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    + "writeArray#" :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    - "sizeofArray#"        :: {"Array#" %a} -> T_Int64
    - "sizeofMutableArray#" :: {"MutableArray#" %s %a} -> T_Int64
    + "indexArray#"         :: {"Array#" %a} -> T_Int64 -> {"GHC.Prim.Unit#" %a}

  primop effectful
    + "unsafeFreezeArray#" :: {"MutableArray#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"Array#" %a}}
    + "unsafeThawArray#"   :: {"Array#" %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    + "copyArray#"         :: {"Array#" %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "copyMutableArray#"  :: {"MutableArray#" %s %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "cloneArray#"        :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"Array#" %a}
    + "cloneMutableArray#" :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    + "freezeArray#"       :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"Array#" %a}}
    + "thawArray#"         :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    + "casArray#"          :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}

  Small Arrays

  primop effectful
    + "newSmallArray#" :: T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}

  primop pure
    - "sameSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> {"SmallMutableArray#" %s %a} -> T_Int64

  primop effectful
    + "readSmallArray#"  :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    + "writeSmallArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    - "sizeofSmallArray#"        :: {"SmallArray#" %a} -> T_Int64
    - "sizeofSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64
    + "indexSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> {"GHC.Prim.Unit#" %a}

  primop effectful
    + "unsafeFreezeSmallArray#" :: {"SmallMutableArray#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallArray#" %a}}
    + "unsafeThawSmallArray#"   :: {"SmallArray#" %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    + "copySmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "copySmallMutableArray#"  :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "cloneSmallArray#"        :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"SmallArray#" %a}
    + "cloneSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    + "freezeSmallArray#"       :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallArray#" %a}}
    + "thawSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    + "casSmallArray#"          :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}
-}
  describe "GHC Array PrimOps" $ do

    it "newArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#" :: (T_Int64) @ t.106 -> %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.2 %a.6} @ t.109} @ t.108
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v05 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  v04
            v05
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array" , [ [ "v03" , "t.109" , "v01" ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v05" , "v03" , "t.109" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "MutableArray#" ]
            ]
          )
        ]

    it "readArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#" :: (T_Int64) @ t.106 -> %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.2 %a.6} @ t.109} @ t.108
            "readArray#" :: {"MutableArray#" %s.21 %a.11} @ t.243 -> (T_Int64) @ t.244 -> {"State#" %s.21} @ t.245 -> {"GHC.Prim.Unit#" %a.11} @ t.246
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = #T_Int64 0
                    v06 = "readArray#" $ v04 v05 v02
                    v08 = case v06 of
                      ("GHC.Prim.Unit#" v07) @ a01 ->
                        v07
                  v08
            v09
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array" , [ [ "v03" , "t.109" , "v01" ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "a01" , "v06" , "t.246" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v06" , "v06" , "t.246" ]
            , [ "v07" , "v06" , "a.11" ]
            , [ "v08" , "v06" , "a.11" ]
            , [ "v09" , "v06" , "a.11" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v05" , "v05" ]
            , [ "v07" , "v01" ]
            , [ "v08" , "v01" ]
            , [ "v09" , "v01" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.Unit#" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "lit:T_Int64" ]
            , [ "v06" , "GHC.Prim.Unit#" ]
            , [ "v07" , "Tup0" ]
            , [ "v08" , "Tup0" ]
            , [ "v09" , "Tup0" ]
            ]
          )
        ]

    it "writeArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#"   :: (T_Int64) @ t.106 -> %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.2 %a.6} @ t.109} @ t.108
            "writeArray#" :: {"MutableArray#" %s.4 %a.9} @ t.115 -> (T_Int64) @ t.116 -> %a.9 -> {"State#" %s.4} @ t.117 -> {"GHC.Prim.(##)"} @ t.118
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = #T_Int64 0
                    v06 = [C1]
                    v07 = v06
                    v08 = "writeArray#" $ v04 v05 v07 v02
                  v08
            v09
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array"
          , [ [ "v03" , "t.109" , "v01" ]
            , [ "v03" , "t.109" , "v07" ]
            ]
          )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v08" , "v08" , "t.118" ]
            , [ "v09" , "v08" , "t.118" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v05" , "v05" ]
            , [ "v06" , "v06" ]
            , [ "v07" , "v06" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "lit:T_Int64" ]
            , [ "v06" , "C1" ]
            , [ "v07" , "C1" ]
            , [ "v08" , "GHC.Prim.(##)" ]
            , [ "v09" , "GHC.Prim.(##)" ]
            ]
          )
        ]

    it "unsafeFreezeArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#"           :: (T_Int64) @ t.00 -> %a.01 -> {"State#" %s.02} @ t.03 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.02 %a.01} @ t.04} @ t.05
            "unsafeFreezeArray#"  :: {"MutableArray#" %s.10 %a.11} @ t.12 -> {"State#" %s.10} @ t.13 -> {"GHC.Prim.Unit#" {"Array#" %a.11} @ t.14} @ t.15
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v08 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = "unsafeFreezeArray#" $ v04 v02
                    v07 = case v05 of
                      ("GHC.Prim.Unit#" v06) @ a01 ->
                        v06
                  v07
            v08
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array"
          , [ [ "v03" , "t.04" , "v01" ]
            ]
          )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.05" ]
            , [ "a01" , "v05" , "t.15" ]
            , [ "v03" , "v03" , "t.05" ]
            , [ "v04" , "v03" , "t.04" ]
            , [ "v05" , "v05" , "t.15" ]
            , [ "v06" , "v03" , "t.04" ]
            , [ "v06" , "v05" , "t.14" ]
            , [ "v07" , "v03" , "t.04" ]
            , [ "v07" , "v05" , "t.14" ]
            , [ "v08" , "v03" , "t.04" ]
            , [ "v08" , "v05" , "t.14" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.Unit#" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "GHC.Prim.Unit#" ]
            , [ "v06" , "Array#" ]
            , [ "v06" , "MutableArray#" ]
            , [ "v07" , "Array#" ]
            , [ "v07" , "MutableArray#" ]
            , [ "v08" , "Array#" ]
            , [ "v08" , "MutableArray#" ]
            ]
          )
        ]

    it "copyMutableArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#"         :: (T_Int64) @ t.00 -> %a.01 -> {"State#" %s.02} @ t.03 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.02 %a.01} @ t.04} @ t.05
            "copyMutableArray#" :: {"MutableArray#" %s.10 %a.11} @ t.12 -> (T_Int64) @ t.13 -> {"MutableArray#" %s.10 %a.11} @ t.14 -> (T_Int64) @ t.15 -> (T_Int64) @ t.16 -> {"State#" %s.10} @ t.17 -> {"GHC.Prim.(##)"} @ t.18
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v11 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = [C1]
                    v06 = "newArray#" $ v00 v05 v02
                    v10 = case v06 of
                      ("GHC.Prim.Unit#" v07) @ a01 ->
                        letS
                          v08 = #T_Int64 0
                          v09 = "copyMutableArray#" $ v04 v08 v07 v08 v00 v02
                        v09
                  v10
            v11
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array"
          , [ [ "v03" , "t.04" , "v01" ]
            , [ "v06" , "t.04" , "v01" ]
            , [ "v06" , "t.04" , "v05" ]
            ]
          )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.05" ]
            , [ "a01" , "v06" , "t.05" ]
            , [ "v03" , "v03" , "t.05" ]
            , [ "v04" , "v03" , "t.04" ]
            , [ "v06" , "v06" , "t.05" ]
            , [ "v07" , "v06" , "t.04" ]
            , [ "v09" , "v09" , "t.18" ]
            , [ "v10" , "v09" , "t.18" ]
            , [ "v11" , "v09" , "t.18" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v05" , "v05" ]
            , [ "v08" , "v08" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.Unit#" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "C1" ]
            , [ "v06" , "GHC.Prim.Unit#" ]
            , [ "v07" , "MutableArray#" ]
            , [ "v08" , "lit:T_Int64" ]
            , [ "v09" , "GHC.Prim.(##)" ]
            , [ "v10" , "GHC.Prim.(##)" ]
            , [ "v11" , "GHC.Prim.(##)" ]
            ]
          )
        ]

    it "cloneArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#"           :: (T_Int64) @ t.00 -> %a.01 -> {"State#" %s.02} @ t.03 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.02 %a.01} @ t.04} @ t.05
            "unsafeFreezeArray#"  :: {"MutableArray#" %s.10 %a.11} @ t.12 -> {"State#" %s.10} @ t.13 -> {"GHC.Prim.Unit#" {"Array#" %a.11} @ t.14} @ t.15
            "cloneArray#"         :: {"Array#" %a.20} @ t.21 -> (T_Int64) @ t.22 -> (T_Int64) @ t.23 -> {"Array#" %a.20} @ t.24
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v10 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = "unsafeFreezeArray#" $ v04 v02
                    v09 = case v05 of
                      ("GHC.Prim.Unit#" v06) @ a01 ->
                        letS
                          v07 = #T_Int64 0
                          v08 = "cloneArray#" $ v06 v07 v00
                        v08
                  v09
            v10
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array"
          , [ [ "v03" , "t.04" , "v01" ]
            , [ "v08" , "t.24" , "v01" ]
            ]
          )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.05" ]
            , [ "a01" , "v05" , "t.15" ]
            , [ "v03" , "v03" , "t.05" ]
            , [ "v04" , "v03" , "t.04" ]
            , [ "v05" , "v05" , "t.15" ]
            , [ "v06" , "v03" , "t.04" ]
            , [ "v06" , "v05" , "t.14" ]
            , [ "v08" , "v08" , "t.24" ]
            , [ "v09" , "v08" , "t.24" ]
            , [ "v10" , "v08" , "t.24" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v07" , "v07" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.Unit#" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "GHC.Prim.Unit#" ]
            , [ "v06" , "Array#" ]
            , [ "v06" , "MutableArray#" ]
            , [ "v07" , "lit:T_Int64" ]
            , [ "v08" , "Array#" ]
            , [ "v09" , "Array#" ]
            , [ "v10" , "Array#" ]
            ]
          )
        ]

    it "cloneMutableArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#"          :: (T_Int64) @ t.00 -> %a.01 -> {"State#" %s.02} @ t.03 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.02 %a.01} @ t.04} @ t.05
            "cloneMutableArray#" :: {"MutableArray#" %s.10 %a.11} @ t.12 -> (T_Int64) @ t.13 -> (T_Int64) @ t.14 -> {"State#" %s.10} @ t.15 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.10 %a.11} @ t.16} @ t.17
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = #T_Int64 0
                    v06 = "cloneMutableArray#" $ v04 v05 v00 v02
                    v08 = case v06 of
                      ("GHC.Prim.Unit#" v07) @ a01 ->
                        v07
                  v08
            v09
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array"
          , [ [ "v03" , "t.04" , "v01" ]
            , [ "v06" , "t.16" , "v01" ]
            ]
          )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.05" ]
            , [ "a01" , "v06" , "t.17" ]
            , [ "v03" , "v03" , "t.05" ]
            , [ "v04" , "v03" , "t.04" ]
            , [ "v06" , "v06" , "t.17" ]
            , [ "v07" , "v06" , "t.16" ]
            , [ "v08" , "v06" , "t.16" ]
            , [ "v09" , "v06" , "t.16" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v05" , "v05" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.Unit#" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "lit:T_Int64" ]
            , [ "v06" , "GHC.Prim.Unit#" ]
            , [ "v07" , "MutableArray#" ]
            , [ "v08" , "MutableArray#" ]
            , [ "v09" , "MutableArray#" ]
            ]
          )
        ]

    it "casArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#" :: (T_Int64) @ t.00 -> %a.01 -> {"State#" %s.02} @ t.03 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.02 %a.01} @ t.04} @ t.05
            "casArray#" :: {"MutableArray#" %s.10 %a.11} @ t.12 -> (T_Int64) @ t.13 -> %a.11 -> %a.11 -> {"State#" %s.10} @ t.14 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.15 %a.11} @ t.16
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v11 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = #T_Int64 0
                    v06 = [C1]
                    v07 = "casArray#" $ v04 v05 v01 v06 v02
                    v10 = case v07 of
                      ("GHC.Prim.(#,#)" v08 v09) @ a01 ->
                        v08
                  v10
            v11
        |]
      addUsedM cfa
      toArrOp cfa `sameAs` Map.fromList
        [ ( "Array"
          , [ [ "v03" , "t.04" , "v01" ]
            , [ "v03" , "t.04" , "v06" ]
            ]
          )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.05" ]
            , [ "a01" , "v07" , "t.16" ]
            , [ "v03" , "v03" , "t.05" ]
            , [ "v04" , "v03" , "t.04" ]
            , [ "v07" , "v07" , "t.16" ]
            , [ "v08" , "v07" , "t.15" ]
            , [ "v09" , "v07" , "a.11" ]
            , [ "v10" , "v07" , "t.15" ]
            , [ "v11" , "v07" , "t.15" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v05" , "v05" ]
            , [ "v06" , "v06" ]
            , [ "v09" , "v01" ]
            , [ "v09" , "v06" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.(#,#)" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutableArray#" ]
            , [ "v05" , "lit:T_Int64" ]
            , [ "v06" , "C1" ]
            , [ "v07" , "GHC.Prim.(#,#)" ]
            , [ "v08" , "lit:T_Int64" ]
            , [ "v09" , "C1" ]
            , [ "v09" , "Tup0" ]
            , [ "v10" , "lit:T_Int64" ]
            , [ "v11" , "lit:T_Int64" ]
            ]
          )
        ]

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
