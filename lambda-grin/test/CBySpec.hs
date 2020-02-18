{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module CBySpec where

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

      toCBy = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------
  describe "Created By Analysis" $ do

    it "force constructor" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog|
          main =
            letS
              v00 = [Tup0]
              v01 = v00 $
              v02 = v01 $
            v02
        |]
      addUsedM cfa
      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["v00", "Tup0"]
              , ["v01", "Tup0"]
              , ["v02", "Tup0"]
              ]
          )
        , ( "NodeOrigin",
              [ ["v00", "v00"]
              , ["v01", "v00"]
              , ["v02", "v00"]
              ]
          )
        ]

    it "force external result" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog|
          primop effectful
            "newArray#" :: (T_Int64) @ t.106 -> %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.2 %a.6} @ t.109} @ t.108
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v04 = v03 $
              v05 = v04 $
              v07 = case v05 of
                ("GHC.Prim.Unit#" v06) @ a00 ->
                  v06
            v07
        |]
      addUsedM cfa
      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin"
          , [ [ "a00" , "v03" , "t.108" ]
            , [ "v03" , "v03" , "t.108" ]
            , [ "v04" , "v03" , "t.108" ]
            , [ "v05" , "v03" , "t.108" ]
            , [ "v06" , "v03" , "t.109" ]
            , [ "v07" , "v03" , "t.109" ]
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
            , [ "v04" , "GHC.Prim.Unit#" ]
            , [ "v05" , "GHC.Prim.Unit#" ]
            , [ "v06" , "MutableArray#" ]
            , [ "v07" , "MutableArray#" ]
            ]
          )
        ]


    it "default pattern" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog|
          main =
            letS
              v00 = [Tup0]
              v01 = [OK1]
              v02 = [OK2]
              v03 = case v00 of
                ("Tup0") @ a00 ->
                  v01
                _ @ a01 ->
                  v02
            v03
        |]
      addUsedM cfa
      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "NodeOrigin"
          , [ [ "a00" , "v00" ]
            , [ "a01" , "v00" ]
            , [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v03" , "v01" ]
            , [ "v03" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "Tup0" ]
            , [ "a01" , "Tup0" ]
            , [ "v00" , "Tup0" ]
            , [ "v01" , "OK1" ]
            , [ "v02" , "OK2" ]
            , [ "v03" , "OK1" ]
            , [ "v03" , "OK2" ]
            ]
          )
        ]
      filterAndSort ["MatchedAlt"] cfa `sameAs` Map.fromList
        [ ( "MatchedAlt",
              [ [ "v03" , "a00"]
              , [ "v03" , "a01"]
              ]
          )
        ]

    it "tagToEnum# workaround" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog|
          primop pure
            "tagToEnum#" :: (T_Int64) @ t.110 -> %a.7
          main =
            letS
              v00 = #T_Int64 0
              v01 = #T_Int64 1
              v02 = #T_Int64 2
              v03 = "tagToEnum#" $ v00
              v04 = case v03 of
                (C1) @ a00 ->
                  v01
                (C2) @ a01 ->
                  v02
            v04
        |]
      addUsedM cfa
      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin"
          , [ [ "v03", "v03", "a.7" ]
            ]
          )
        , ( "NodeOrigin"
          , [ [ "v00" , "v00" ]
            , [ "v01" , "v01" ]
            , [ "v02" , "v02" ]
            , [ "v04" , "v01" ]
            , [ "v04" , "v02" ]
            ]
          )
        , ( "TagValue"
          , [ [ "a00" , "C1" ]
            , [ "a01" , "C2" ]
            , [ "v00" , "lit:T_Int64" ]
            , [ "v01" , "lit:T_Int64" ]
            , [ "v02" , "lit:T_Int64" ]
            , [ "v04" , "lit:T_Int64" ]
            ]
          )
        ]
      filterAndSort ["MatchedAlt"] cfa `sameAs` Map.fromList
        [ ( "MatchedAlt",
              [ [ "v04" , "a00"]
              , [ "v04" , "a01"]
              ]
          )
        ]

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
