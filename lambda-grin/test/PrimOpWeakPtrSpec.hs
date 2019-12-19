{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpWeakPtrSpec where

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

      toMVOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "WeakPtr", "WeakFinalizer"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------

  describe "GHC WeakPtr PrimOps" $ do

    it "mkWeak#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "mkWeak#" :: %o.1 -> %b.2 -> (tf.3 : {"State#" {RealWorld} @ t.4} @ t.5 -> {"GHC.Prim.Unit#" %c.6} @ t.7) -> {"State#" {RealWorld} @ t.8} @ t.9 -> {"GHC.Prim.Unit#" {"Weak#" %b.12} @ t.10} @ t.11

          main =
            letS
              v01 = [Key]
              v02 = [Value]
              v03 = #T_Token "RealWorld"
              v04 = "mkWeak#" $ v01 v02 fun1 v03
              v06 = case v04 of
                ("GHC.Prim.Unit#" v05) @ a00 ->
                  v05
            v06
          fun1 p10 =
            letS
              v10 = [Tup0]
              v11 = ["GHC.Prim.Unit#" v10]
            v11

        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "WeakPtr" , [ [ "v04" , "t.10" , "v02" ] ] )
        , ( "WeakFinalizer" , [ [ "v04" , "fun1" ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v04" , "t.11" ]
            , [ "v04" , "v04" , "t.11" ]
            , [ "v05" , "v04" , "t.10" ]
            , [ "v06" , "v04" , "t.10" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "mkWeak#-finalizer-ignored-result" , "v11" ]
            , [ "p10" , "v03" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v03" , "v03" ]
            , [ "v10" , "v10" ]
            , [ "v11" , "v11" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "mkWeak#-finalizer-ignored-result" , "GHC.Prim.Unit#" ]
            , [ "p10" , "lit:T_Token \"RealWorld\"" ]
            , [ "v01" , "Key" ]
            , [ "v02" , "Value" ]
            , [ "v03" , "lit:T_Token \"RealWorld\"" ]
            , [ "v04" , "GHC.Prim.Unit#" ]
            , [ "v05" , "Weak#" ]
            , [ "v06" , "Weak#" ]
            , [ "v10" , "Tup0" ]
            , [ "v11" , "GHC.Prim.Unit#" ]
            ]
          )
        ]

    it "mkWeakNoFinalizer#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "mkWeakNoFinalizer#" :: %o.1 -> %b.2 -> {"State#" {RealWorld} @ t.8} @ t.9 -> {"GHC.Prim.Unit#" {"Weak#" %b.12} @ t.10} @ t.11

          main =
            letS
              v01 = [Key]
              v02 = [Value]
              v03 = #T_Token "RealWorld"
              v04 = "mkWeakNoFinalizer#" $ v01 v02 v03
              v06 = case v04 of
                ("GHC.Prim.Unit#" v05) @ a00 ->
                  v05
            v06
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "WeakPtr" , [ [ "v04" , "t.10" , "v02" ] ] )
        , ( "WeakFinalizer" , [ ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v04" , "t.11" ]
            , [ "v04" , "v04" , "t.11" ]
            , [ "v05" , "v04" , "t.10" ]
            , [ "v06" , "v04" , "t.10" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v03" , "v03" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "v01" , "Key" ]
            , [ "v02" , "Value" ]
            , [ "v03" , "lit:T_Token \"RealWorld\"" ]
            , [ "v04" , "GHC.Prim.Unit#" ]
            , [ "v05" , "Weak#" ]
            , [ "v06" , "Weak#" ]
            ]
          )
        ]

    it "deRefWeak#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "mkWeakNoFinalizer#" :: %o.1 -> %b.2 -> {"State#" {RealWorld} @ t.8} @ t.9 -> {"GHC.Prim.Unit#" {"Weak#" %b.12} @ t.10} @ t.11
            "deRefWeak#"         :: {"Weak#" %a.24} @ t.20 -> {"State#" {RealWorld} @ t.21} @ t.22 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.23 %a.24} @ t.25

          main =
            letS
              v01 = [Key]
              v02 = [Value]
              v03 = #T_Token "RealWorld"
              v04 = "mkWeakNoFinalizer#" $ v01 v02 v03
              v10 = case v04 of
                ("GHC.Prim.Unit#" v05) @ a00 ->
                  letS
                    v06 = "deRefWeak#" $ v05 v03
                    v09 = case v06 of
                      ("GHC.Prim.(#,#)" v07 v08) @ a01 ->
                        v07
                  v09
            v10
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "WeakPtr" , [ [ "v04" , "t.10" , "v02" ] ] )
        , ( "WeakFinalizer" , [ ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v04" , "t.11" ]
            , [ "a01" , "v06" , "t.25" ]
            , [ "v04" , "v04" , "t.11" ]
            , [ "v05" , "v04" , "t.10" ]
            , [ "v06" , "v06" , "t.25" ]
            , [ "v07" , "v06" , "t.23" ]
            , [ "v08" , "v06" , "a.24" ]
            , [ "v09" , "v06" , "t.23" ]
            , [ "v10" , "v06" , "t.23" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v03" , "v03" ]
            , [ "v08" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.(#,#)" ]
            , [ "v01" , "Key" ]
            , [ "v02" , "Value" ]
            , [ "v03" , "lit:T_Token \"RealWorld\"" ]
            , [ "v04" , "GHC.Prim.Unit#" ]
            , [ "v05" , "Weak#" ]
            , [ "v06" , "GHC.Prim.(#,#)" ]
            , [ "v07" , "lit:T_Int64" ]
            , [ "v08" , "Value" ]
            , [ "v09" , "lit:T_Int64" ]
            , [ "v10" , "lit:T_Int64" ]
            ]
          )
        ]

    it "finalizeWeak#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "mkWeak#" :: %o.1 -> %b.2 -> (tf.3 : {"State#" {RealWorld} @ t.4} @ t.5 -> {"GHC.Prim.Unit#" %c.6} @ t.7) -> {"State#" {RealWorld} @ t.8} @ t.9 -> {"GHC.Prim.Unit#" {"Weak#" %b.12} @ t.10} @ t.11
            "finalizeWeak#"  :: {"Weak#" %a.20} @ t.21 -> {"State#" {RealWorld} @ t.22} @ t.23 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.24 (tf.25 : {"State#" {RealWorld} @ t.26} @ t.27 -> {"GHC.Prim.Unit#" %b} @ t.28)} @ t.29

          main =
            letS
              v01 = [Key]
              v02 = [Value]
              v03 = #T_Token "RealWorld"
              v04 = "mkWeak#" $ v01 v02 fun1 v03
              v010 = case v04 of
                ("GHC.Prim.Unit#" v05) @ a00 ->
                  letS
                    v06 = "finalizeWeak#" $ v05 v03
                    v09 = case v06 of
                      ("GHC.Prim.(#,#)" v07 v08) @ a01 ->
                        v07
                  v09
            v010
          fun1 p10 =
            letS
              v10 = [Tup0]
              v11 = ["GHC.Prim.Unit#" v10]
            v11

        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "WeakPtr" , [ [ "v04" , "t.10" , "v02" ] ] )
        , ( "WeakFinalizer" , [ [ "v04" , "fun1" ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v04" , "t.11" ]
            , [ "a01" , "v06" , "t.29" ]
            , [ "v010", "v06" , "t.24" ]
            , [ "v04" , "v04" , "t.11" ]
            , [ "v05" , "v04" , "t.10" ]
            , [ "v06" , "v06" , "t.29" ]
            , [ "v07" , "v06" , "t.24" ]
            , [ "v08" , "v06" , "tf.25" ]
            , [ "v09" , "v06" , "t.24" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "mkWeak#-finalizer-ignored-result" , "v11" ]
            , [ "p10" , "v03" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v03" , "v03" ]
            , [ "v10" , "v10" ]
            , [ "v11" , "v11" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.(#,#)" ]
            , [ "mkWeak#-finalizer-ignored-result" , "GHC.Prim.Unit#" ]
            , [ "p10" , "lit:T_Token \"RealWorld\"" ]
            , [ "v01" , "Key" ]
            , [ "v010", "lit:T_Int64" ]
            , [ "v02" , "Value" ]
            , [ "v03" , "lit:T_Token \"RealWorld\"" ]
            , [ "v04" , "GHC.Prim.Unit#" ]
            , [ "v05" , "Weak#" ]
            , [ "v06" , "GHC.Prim.(#,#)" ]
            , [ "v07" , "lit:T_Int64" ]
            , [ "v09" , "lit:T_Int64" ]
            , [ "v10" , "Tup0" ]
            , [ "v11" , "GHC.Prim.Unit#" ]
            ]
          )
        ]
      filterAndSort ["PNode", "ApplyChain"] cfa `sameAs` Map.fromList
        [ ( "ApplyChain" , [] )
        , ( "PNode",
              [ [ "fun1", "fun1" , "1" , "1" ]
              , [ "v08" , "fun1" , "1" , "1" ]
              ]
          )
        ]

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
