{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpExceptionSpec where

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

      toExOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "RaisedEx"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------

  describe "GHC Exception PrimOps" $ do

    {-
    + "catch#"                 :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> (%b -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    + "raise#"                 :: %b -> %o
    + "raiseIO#"               :: %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %b}
    + "maskAsyncExceptions#"   :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    + "maskUninterruptible#"   :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    + "unmaskAsyncExceptions#" :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    - "getMaskingState#"       :: {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Int64}
    -}

    it "catch - raise" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "catch#" :: (tf.0 : {"State#" {RealWorld} @ t.1} @ t.0 -> {"GHC.Prim.Unit#" %a.0} @ t.2) -> (tf.1 : %b.0 -> {"State#" {RealWorld} @ t.4} @ t.3 -> {"GHC.Prim.Unit#" %a.0} @ t.5) -> {"State#" {RealWorld} @ t.7} @ t.6 -> {"GHC.Prim.Unit#" %a.0} @ t.8
            "raise#" :: %b.1 -> %o.0
          main =
            letS
              v00 = #T_Token "RealWorld"
              v01 = "catch#" $ fun1_normal fun2_handler v00
              v02 = case v01 of
                ("GHC.Prim.Unit#" v03) @ a00 ->
                  v03
            v02
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
      toExOp cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin"
          , [ [ "a00" , "v01" , "t.8" ]
            , [ "v01" , "v01" , "t.8" ]
            , [ "v01" , "v11" , "o.0" ]
            , [ "v02" , "v01" , "a.0" ]
            , [ "v03" , "v01" , "a.0" ]
            , [ "v11" , "v11" , "o.0" ]
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

    it "catch - raiseIO" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "catch#"    :: (tf.0 : {"State#" {RealWorld} @ t.1} @ t.0 -> {"GHC.Prim.Unit#" %a.0} @ t.2) -> (tf.1 : %b.0 -> {"State#" {RealWorld} @ t.4} @ t.3 -> {"GHC.Prim.Unit#" %a.0} @ t.5) -> {"State#" {RealWorld} @ t.7} @ t.6 -> {"GHC.Prim.Unit#" %a.0} @ t.8
            "raiseIO#"  :: %a.3 -> {"State#" {RealWorld} @ t.28} @ t.27 -> {"GHC.Prim.Unit#" %b.2} @ t.29
          main =
            letS
              v00 = #T_Token "RealWorld"
              v01 = "catch#" $ fun1_normal fun2_handler v00
              v02 = case v01 of
                ("GHC.Prim.Unit#" v03) @ a00 ->
                  v03
            v02
          fun1_normal p10 =
            letS
              v10 = #T_Int64 0
              v11 = #T_Token "RealWorld"
              v12 = "raiseIO#" $ v10 v11
            v12
          fun2_handler p20 p21 =
            letS
              v20 = #T_Int64 0
              v21 = ["GHC.Prim.Unit#" v20]
            v21
        |]
      addUsedM cfa
      toExOp cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin"
          , [ [ "a00" , "v01" , "t.8" ]
            , [ "a00" , "v12" , "t.29" ]
            , [ "v01" , "v01" , "t.8" ]
            , [ "v01" , "v12" , "t.29" ]
            , [ "v02" , "v01" , "a.0" ]
            , [ "v02" , "v12" , "b.2" ]
            , [ "v03" , "v01" , "a.0" ]
            , [ "v03" , "v12" , "b.2" ]
            , [ "v12" , "v12" , "t.29" ]
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
            , [ "v11" , "v11" ]
            , [ "v20" , "v20" ]
            , [ "v21" , "v21" ]
            ]
          )
        , ( "RaisedEx" , [ [ "v10" ] ] )
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
            , [ "v11" , "lit:T_Token \"RealWorld\"" ]
            , [ "v12" , "GHC.Prim.Unit#" ]
            , [ "v20" , "lit:T_Int64" ]
            , [ "v21" , "GHC.Prim.Unit#" ]
            ]
          )
        ]

    it "maskAsyncExceptions#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "maskAsyncExceptions#" :: (tf.4 : {"State#" {RealWorld} @ t.14} @ t.13 -> {"GHC.Prim.Unit#" %a.1} @ t.15) -> {"State#" {RealWorld} @ t.17} @ t.16 -> {"GHC.Prim.Unit#" %a.1} @ t.18
          main =
            letS
              v00 = #T_Token "RealWorld"
              v01 = "maskAsyncExceptions#" $ fun1 v00
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
      toExOp cfa `sameAs` Map.fromList
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

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
