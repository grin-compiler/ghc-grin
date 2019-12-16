{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpSTMSpec where

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

      toSTMOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "RaisedEx", "TVar"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------

  describe "GHC STM PrimOps" $ do

    it "atomically#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "atomically#" :: (tf.4 : {"State#" {RealWorld} @ t.14} @ t.13 -> {"GHC.Prim.Unit#" %a.1} @ t.15) -> {"State#" {RealWorld} @ t.17} @ t.16 -> {"GHC.Prim.Unit#" %a.1} @ t.18
          main =
            letS
              v00 = #T_Token "RealWorld"
              v01 = "atomically#" $ fun1 v00
              v02 = case v01 of
                ("GHC.Prim.Unit#" v03) @ a00 ->
                  v03
            v02
          fun1 p10 =
            letS
              v10 = #T_Int64 0
              v11 = ["GHC.Prim.Unit#" v10]
            v11
        |]
      addUsedM cfa
      toSTMOp cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin"
          , [ [ "a00" , "v01" , "t.18" ]
            , [ "v01" , "v01" , "t.18" ]
            , [ "v02" , "v01" , "a.1" ]
            , [ "v03" , "v01" , "a.1" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "a00" , "v11" ]
            , [ "p10" , "v00" ]
            , [ "v00" , "v00" ]
            , [ "v01" , "v11" ]
            , [ "v02" , "v10" ]
            , [ "v03" , "v10" ]
            , [ "v10" , "v10" ]
            , [ "v11" , "v11" ]
            ]
          )
        , ( "RaisedEx" , [] )
        , ( "TVar", [] )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "p10" , "lit:T_Token \"RealWorld\"" ]
            , [ "v00" , "lit:T_Token \"RealWorld\"" ]
            , [ "v01" , "GHC.Prim.Unit#" ]
            , [ "v02" , "lit:T_Int64" ]
            , [ "v03" , "lit:T_Int64" ]
            , [ "v10" , "lit:T_Int64" ]
            , [ "v11" , "GHC.Prim.Unit#" ]
            ]
          )
        ]

    it "catchRetry#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "catchRetry#" :: (tf.0 : {"State#" {RealWorld} @ t.1} @ t.0 -> {"GHC.Prim.Unit#" %a.0} @ t.2) -> (tf.1 : {"State#" {RealWorld} @ t.4} @ t.3 -> {"GHC.Prim.Unit#" %a.0} @ t.5) -> {"State#" {RealWorld} @ t.7} @ t.6 -> {"GHC.Prim.Unit#" %a.0} @ t.8
          main =
            letS
              v00 = #T_Token "RealWorld"
              v01 = "catchRetry#" $ fun1_normal fun2_handler v00
              v03 = case v01 of
                ("GHC.Prim.Unit#" v02) @ a00 ->
                  v02
            v03
          fun1_normal p10 =
            letS
              v10 = #T_Int64 0
              v11 = ["GHC.Prim.Unit#" v10]
            v11
          fun2_handler p20 =
            letS
              v20 = #T_Int64 0
              v21 = ["GHC.Prim.Unit#" v20]
            v21
        |]
      addUsedM cfa
      toSTMOp cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin"
          , [ [ "a00" , "v01" , "t.8" ]
            , [ "v01" , "v01" , "t.8" ]
            , [ "v02" , "v01" , "a.0" ]
            , [ "v03" , "v01" , "a.0" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "a00" , "v11" ]
            , [ "a00" , "v21" ]
            , [ "p10" , "v00" ]
            , [ "p20" , "v00" ]
            , [ "v00" , "v00" ]
            , [ "v01" , "v11" ]
            , [ "v01" , "v21" ]
            , [ "v02" , "v10" ]
            , [ "v02" , "v20" ]
            , [ "v03" , "v10" ]
            , [ "v03" , "v20" ]
            , [ "v10" , "v10" ]
            , [ "v11" , "v11" ]
            , [ "v20" , "v20" ]
            , [ "v21" , "v21" ]
            ]
          )
        , ( "RaisedEx" , [ ] )
        , ( "TVar", [] )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "p10" , "lit:T_Token \"RealWorld\"" ]
            , [ "p20" , "lit:T_Token \"RealWorld\"" ]
            , [ "v00" , "lit:T_Token \"RealWorld\"" ]
            , [ "v01" , "GHC.Prim.Unit#" ]
            , [ "v02" , "lit:T_Int64" ]
            , [ "v03" , "lit:T_Int64" ]
            , [ "v10" , "lit:T_Int64" ]
            , [ "v11" , "GHC.Prim.Unit#" ]
            , [ "v20" , "lit:T_Int64" ]
            , [ "v21" , "GHC.Prim.Unit#" ]
            ]
          )
        ]

    it "catchSTM# - raise#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "catchSTM#" :: (tf.0 : {"State#" {RealWorld} @ t.1} @ t.0 -> {"GHC.Prim.Unit#" %a.0} @ t.2) -> (tf.1 : %b.0 -> {"State#" {RealWorld} @ t.4} @ t.3 -> {"GHC.Prim.Unit#" %a.0} @ t.5) -> {"State#" {RealWorld} @ t.7} @ t.6 -> {"GHC.Prim.Unit#" %a.0} @ t.8
            "raise#"    :: %a.3 -> %b.2
          main =
            letS
              v00 = #T_Token "RealWorld"
              v01 = "catchSTM#" $ fun1_normal fun2_handler v00
              v03 = case v01 of
                ("GHC.Prim.Unit#" v02) @ a00 ->
                  v02
            v03
          fun1_normal p10 =
            letS
              v10 = #T_Int64 0
              v11 = "raise#" $ v10
            v11
          fun2_handler p20 p21 =
            letS
              v20 = #T_Int64 0
              v21 = ["GHC.Prim.Unit#" v20]
            v21
        |]
      addUsedM cfa
      toSTMOp cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin"
          , [ [ "a00" , "v01" , "t.8" ]
            , [ "v01" , "v01" , "t.8" ]
            , [ "v01" , "v11" , "b.2" ]
            , [ "v02" , "v01" , "a.0" ]
            , [ "v03" , "v01" , "a.0" ]
            , [ "v11" , "v11" , "b.2" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "a00" , "v21" ]
            , [ "p10" , "v00" ]
            , [ "p20" , "v10" ]
            , [ "p21" , "v00" ]
            , [ "v00" , "v00" ]
            , [ "v01" , "v21" ]
            , [ "v02" , "v20" ]
            , [ "v03" , "v20" ]
            , [ "v10" , "v10" ]
            , [ "v20" , "v20" ]
            , [ "v21" , "v21" ]
            ]
          )
        , ( "RaisedEx" , [ [ "v10" ] ] )
        , ( "TVar", [] )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "p10" , "lit:T_Token \"RealWorld\"" ]
            , [ "p20" , "lit:T_Int64" ]
            , [ "p21" , "lit:T_Token \"RealWorld\"" ]
            , [ "v00" , "lit:T_Token \"RealWorld\"" ]
            , [ "v01" , "GHC.Prim.Unit#" ]
            , [ "v02" , "lit:T_Int64" ]
            , [ "v03" , "lit:T_Int64" ]
            , [ "v10" , "lit:T_Int64" ]
            , [ "v20" , "lit:T_Int64" ]
            , [ "v21" , "GHC.Prim.Unit#" ]
            ]
          )
        ]

    it "newTVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newTVar#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"TVar#" %s.2 %a.6} @ t.109} @ t.108
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newTVar#" $ v01 v02
              v05 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  v04
            v05
        |]
      addUsedM cfa
      toSTMOp cfa `sameAs` Map.fromList
        [ ( "TVar" , [ [ "v03" , "t.109" , "v01" ] ] )
        , ( "RaisedEx" , [ ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v05" , "v03" , "t.109" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "TVar#" ]
            , [ "v05" , "TVar#" ]
            ]
          )
        ]

    it "readTVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newTVar#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"TVar#" %s.2 %a.6} @ t.109} @ t.108
            "readTVar#" :: {"TVar#" %s.21 %a.11} @ t.243 -> {"State#" %s.21} @ t.245 -> {"GHC.Prim.Unit#" %a.11} @ t.246
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newTVar#" $ v01 v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v06 = "readTVar#" $ v04 v02
                    v08 = case v06 of
                      ("GHC.Prim.Unit#" v07) @ a01 ->
                        v07
                  v08
            v09
        |]
      addUsedM cfa
      toSTMOp cfa `sameAs` Map.fromList
        [ ( "TVar" , [ [ "v03" , "t.109" , "v01" ] ] )
        , ( "RaisedEx" , [ ] )
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
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v07" , "v01" ]
            , [ "v08" , "v01" ]
            , [ "v09" , "v01" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.Unit#" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "TVar#" ]
            , [ "v06" , "GHC.Prim.Unit#" ]
            , [ "v07" , "Tup0" ]
            , [ "v08" , "Tup0" ]
            , [ "v09" , "Tup0" ]
            ]
          )
        ]

    it "writeTVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newTVar#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"TVar#" %s.2 %a.6} @ t.109} @ t.108
            "writeTVar#" :: {"TVar#" %s.4 %a.9} @ t.115 -> %a.9 -> {"State#" %s.4} @ t.117 -> {"GHC.Prim.(##)"} @ t.118
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newTVar#" $ v01 v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v06 = [C1]
                    v08 = "writeTVar#" $ v04 v06 v02
                  v08
            v09
        |]
      addUsedM cfa
      toSTMOp cfa `sameAs` Map.fromList
        [ ( "TVar"
          , [ [ "v03" , "t.109" , "v01" ]
            , [ "v03" , "t.109" , "v06" ]
            ]
          )
        , ( "RaisedEx" , [ ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v08" , "v08" , "t.118" ]
            , [ "v09" , "v08" , "t.118" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v06" , "v06" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "TVar#" ]
            , [ "v06" , "C1" ]
            , [ "v08" , "GHC.Prim.(##)" ]
            , [ "v09" , "GHC.Prim.(##)" ]
            ]
          )
        ]

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
