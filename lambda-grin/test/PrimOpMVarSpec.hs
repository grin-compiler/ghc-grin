{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpMVarSpec where

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

      toMVOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "MVar", "MVarDef"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------

  describe "GHC MVar PrimOps" $ do

    it "newMVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newMVar#" :: {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MVar#" %s.2 %a.6} @ t.109} @ t.108

          main =
            letS
              v02 = #T_Token "RealWorld"
              v03 = "newMVar#" $ v02
              v05 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  v04
            v05
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "MVar" , [] )
        , ( "MVarDef" , [ [ "v03" , "t.109" ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v05" , "v03" , "t.109" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v02" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MVar#" ]
            , [ "v05" , "MVar#" ]
            ]
          )
        ]

    it "putMVar# - readMVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newMVar#"  :: {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MVar#" %s.2 %a.6} @ t.109} @ t.108
            "readMVar#" :: {"MVar#" %s.21 %a.11} @ t.243 -> {"State#" %s.21} @ t.245 -> {"GHC.Prim.Unit#" %a.11} @ t.246
            "putMVar#"  :: {"MVar#" %s.4 %a.9} @ t.115 -> %a.9 -> {"State#" %s.4} @ t.117 -> {"GHC.Prim.(##)"} @ t.118

          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newMVar#" $ v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = "putMVar#" $ v04 v01 v02
                    v06 = "readMVar#" $ v04 v02
                    v08 = case v06 of
                      ("GHC.Prim.Unit#" v07) @ a01 ->
                        v07
                  v08
            v09
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "MVar" ,    [ [ "v03" , "t.109" , "v01" ] ] )
        , ( "MVarDef" , [ [ "v03" , "t.109"         ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "a01" , "v06" , "t.246" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v05" , "v05" , "t.118" ]
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
            , [ "v04" , "MVar#" ]
            , [ "v05" , "GHC.Prim.(##)" ]
            , [ "v06" , "GHC.Prim.Unit#" ]
            , [ "v07" , "Tup0" ]
            , [ "v08" , "Tup0" ]
            , [ "v09" , "Tup0" ]
            ]
          )
        ]

    it "putMVar# - tryReadMVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newMVar#"      :: {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MVar#" %s.2 %a.6} @ t.109} @ t.108
            "tryReadMVar#"  :: {"MVar#" %s.21 %a.11} @ t.243 -> {"State#" %s.21} @ t.245 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.242 %a.11} @ t.246
            "putMVar#"      :: {"MVar#" %s.4 %a.9} @ t.115 -> %a.9 -> {"State#" %s.4} @ t.117 -> {"GHC.Prim.(##)"} @ t.118

          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newMVar#" $ v02
              v10 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v05 = "putMVar#" $ v04 v01 v02
                    v06 = "tryReadMVar#" $ v04 v02
                    v09 = case v06 of
                      ("GHC.Prim.(#,#)" v07 v08) @ a01 ->
                        v08
                  v09
            v10
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "MVar" ,    [ [ "v03" , "t.109" , "v01" ] ] )
        , ( "MVarDef" , [ [ "v03" , "t.109"         ] ] )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "a01" , "v06" , "t.246" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v05" , "v05" , "t.118" ]
            , [ "v06" , "v06" , "t.246" ]
            , [ "v07" , "v06" , "t.242" ]
            , [ "v08" , "v06" , "a.11" ]
            , [ "v09" , "v06" , "a.11" ]
            , [ "v10" , "v06" , "a.11" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v08" , "v01" ]
            , [ "v09" , "v01" ]
            , [ "v10" , "v01" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.(#,#)" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MVar#" ]
            , [ "v05" , "GHC.Prim.(##)" ]
            , [ "v06" , "GHC.Prim.(#,#)" ]
            , [ "v07" , "lit:T_Int64" ]
            , [ "v08" , "Tup0" ]
            , [ "v09" , "Tup0" ]
            , [ "v10" , "Tup0" ]
            ]
          )
        ]

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
