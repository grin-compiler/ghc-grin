{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpMutVarSpec where

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

      toMVOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "MutVar"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------

  describe "GHC MutVar PrimOps" $ do

    it "newMutVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newMutVar#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutVar#" %s.2 %a.6} @ t.109} @ t.108
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newMutVar#" $ v01 v02
              v05 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  v04
            v05
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "MutVar" , [ [ "v03" , "t.109" , "v01" ] ] )
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
            , [ "v04" , "MutVar#" ]
            , [ "v05" , "MutVar#" ]
            ]
          )
        ]

    it "readMutVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newMutVar#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutVar#" %s.2 %a.6} @ t.109} @ t.108
            "readMutVar#" :: {"MutVar#" %s.21 %a.11} @ t.243 -> {"State#" %s.21} @ t.245 -> {"GHC.Prim.Unit#" %a.11} @ t.246
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newMutVar#" $ v01 v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v06 = "readMutVar#" $ v04 v02
                    v08 = case v06 of
                      ("GHC.Prim.Unit#" v07) @ a01 ->
                        v07
                  v08
            v09
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "MutVar" , [ [ "v03" , "t.109" , "v01" ] ] )
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
            , [ "v04" , "MutVar#" ]
            , [ "v06" , "GHC.Prim.Unit#" ]
            , [ "v07" , "Tup0" ]
            , [ "v08" , "Tup0" ]
            , [ "v09" , "Tup0" ]
            ]
          )
        ]

    it "writeMutVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newMutVar#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutVar#" %s.2 %a.6} @ t.109} @ t.108
            "writeMutVar#" :: {"MutVar#" %s.4 %a.9} @ t.115 -> %a.9 -> {"State#" %s.4} @ t.117 -> {"GHC.Prim.(##)"} @ t.118
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newMutVar#" $ v01 v02
              v09 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v06 = [C1]
                    v08 = "writeMutVar#" $ v04 v06 v02
                  v08
            v09
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "MutVar"
          , [ [ "v03" , "t.109" , "v01" ]
            , [ "v03" , "t.109" , "v06" ]
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
            , [ "v04" , "MutVar#" ]
            , [ "v06" , "C1" ]
            , [ "v08" , "GHC.Prim.(##)" ]
            , [ "v09" , "GHC.Prim.(##)" ]
            ]
          )
        ]

    it "casMutVar#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newMutVar#" :: %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutVar#" %s.2 %a.6} @ t.109} @ t.108
            "casMutVar#" :: {"MutVar#" %s.10 %a.11} @ t.12 -> %a.11 -> %a.11 -> {"State#" %s.10} @ t.14 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.15 %a.11} @ t.16
          main =
            letS
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newMutVar#" $ v01 v02
              v11 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  letS
                    v06 = [C1]
                    v07 = "casMutVar#" $ v04 v01 v06 v02
                    v10 = case v07 of
                      ("GHC.Prim.(#,#)" v08 v09) @ a01 ->
                        v08
                  v10
            v11
        |]
      addUsedM cfa
      toMVOp cfa `sameAs` Map.fromList
        [ ( "MutVar"
          , [ [ "v03" , "t.109" , "v01" ]
            , [ "v03" , "t.109" , "v06" ]
            ]
          )
        , ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "a01" , "v07" , "t.16"  ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.109" ]
            , [ "v07" , "v07" , "t.16" ]
            , [ "v08" , "v07" , "t.15" ]
            , [ "v09" , "v07" , "a.11" ]
            , [ "v10" , "v07" , "t.15" ]
            , [ "v11" , "v07" , "t.15" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v06" , "v06" ]
            , [ "v09" , "v01" ]
            , [ "v09" , "v06" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "GHC.Prim.Unit#" ]
            , [ "a01" , "GHC.Prim.(#,#)" ]
            , [ "v01" , "Tup0" ]
            , [ "v02" , "lit:T_Token \"RealWorld\"" ]
            , [ "v03" , "GHC.Prim.Unit#" ]
            , [ "v04" , "MutVar#" ]
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
