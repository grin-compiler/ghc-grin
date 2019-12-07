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
      pPrint $ toExOp cfa
{-
      toExOp cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["v0", "lit:T_Int64"]
              , ["v1", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["v0", "v0"]
              , ["v1", "v0"]
              ]
          )
        ]
-}
    pure ()

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
