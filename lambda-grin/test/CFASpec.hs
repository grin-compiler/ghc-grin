{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module CFASpec where

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

  describe "CBy - lit - known - saturated" $ do


    it "var ; move" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v0 = #T_Int64 0
              v1 = v0
            v1
        |]
      addUsedM cfa

      toCBy cfa `shouldBe` Map.fromList
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

    it "alt value ; pattern match" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = case v00 of
                #T_Int64 0 @ a00 ->
                  letS
                    v10 = a00
                  v10
            v01
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["a00", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["a00", "v00"]
              , ["v00", "v00"]
              , ["v01", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    it "alt value ; pattern non-match" $ do
      -- NOTE: CreatedBy analysis does not track literal values
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = case v00 of
                #T_Int64 1 @ a00 ->
                  letS
                    v10 = a00
                  v10
            v01
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["a00", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["a00", "v00"]
              , ["v00", "v00"]
              , ["v01", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    xit "alt value ; default" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = case v00 of
                _ @ a00 ->
                  letS
                    v10 = a00
                  v10
            v01
        |]

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["a00", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["a00", "v00"]
              , ["v00", "v00"]
              , ["v01", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    xit "alt value ; all in one" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = case v00 of
                _ @ a00 ->
                  letS
                    v10 = a00
                  v10
                #T_Int64 0 @ a20 ->
                  letS
                    v20 = a20
                  v20
                #T_Int64 1 @ a30 ->
                  letS
                    v30 = a30
                  v30
            v01
        |]
      pure ()

    it "fun param & result" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun $ v00
            v01
          fun p10 =
            letS
              v10 = p10
            v10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["v00", "v00"]
              , ["v01", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    it "fun param & result ; flow & context insensitive imprecision" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun $ v00
              v02 = #T_Word64 1
              v03 = fun $ v02
            v01
          fun p10 =
            letS
              v10 = p10
            v10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["p10", "lit:T_Word64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v01", "lit:T_Word64"]
              , ["v02", "lit:T_Word64"]
              , ["v03", "lit:T_Int64"]
              , ["v03", "lit:T_Word64"]
              , ["v10", "lit:T_Int64"]
              , ["v10", "lit:T_Word64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["p10", "v02"]
              , ["v00", "v00"]
              , ["v01", "v00"]
              , ["v01", "v02"]
              , ["v02", "v02"]
              , ["v03", "v00"]
              , ["v03", "v02"]
              , ["v10", "v00"]
              , ["v10", "v02"]
              ]
          )
        ]

    it "closure param & result" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            let
              clo = \[] p10 ->
                letS
                  v10 = p10
                v10
            letS
              v00 = #T_Int64 0
              v01 = clo $ v00
            v01
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["v00", "v00"]
              , ["v01", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

  ----------------------------

  describe "CBy - lit - unknown - saturated" $ do

    it "fun param" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun2 $ fun1 v00
            v01
          fun2 p20 p21 =
            letS
              v20 = p20 $ p21
            v20
          fun1 p10 =
            letS
              v10 = p10
            v10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["p21", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              , ["v20", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["p21", "v00"]
              , ["v00", "v00"]
              , ["v01", "v00"]
              , ["v10", "v00"]
              , ["v20", "v00"]
              ]
          )
        ]

    it "fun param & result ; fun as closure, MOVE" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun2 $
              v02 = v01 $ v00
            v02
          fun2 =
            letS
              v20 = fun1 -- MOVE
            v20
          fun1 p10 =
            letS
              v10 = p10
            v10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["v00", "v00"]
              , ["v02", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    it "fun param & result ; fun as closure, saturated + undersaturated, CALL" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun2 $
              v02 = v01 $ v00
            v02
          fun2 =
            letS
              v20 = fun1 $ -- undersaturated known call, does nothing
            v20
          fun1 p10 =
            letS
              v10 = p10
            v10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["v00", "v00"]
              , ["v02", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    it "closure param & result " $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun2 $
              v02 = v01 $ v00
            v02
          fun2 =
            let
              c20 = \[] p20 ->
                letS
                  v20 = p20
                v20
            c20
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p20", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              , ["v20", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p20", "v00"]
              , ["v00", "v00"]
              , ["v02", "v00"]
              , ["v20", "v00"]
              ]
          )
        ]

    it "closure param & result ; var result" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun2 $
              v02 = v01 $ v00
            v02
          fun2 =
            let
              c20 = \[] p20 ->
                p20
            c20
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p20", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p20", "v00"]
              , ["v00", "v00"]
              , ["v02", "v00"]
              ]
          )
        ]

  ----------------------------

  describe "CBy - lit - unknown - undersaturated" $ do

    it "fun param & result ; context insenisitve return value mix" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun1_ap $
              v02 = v01 $ v01
              v03 = fun2_id $
              v04 = v02 $ v03
              v05 = v04 $ v00
            v05
          fun1_ap p10 p11 =
            letS
              v10 = p10 $ p11
            v10
          fun2_id p20 =
            p20
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["p11", "lit:T_Int64"]
              , ["p20", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v04", "lit:T_Int64"]
              , ["v05", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ [ "p10" , "v00" ]
              , [ "p11" , "v00" ]
              , [ "p20" , "v00" ]
              , [ "v00" , "v00" ]
              , [ "v04" , "v00" ]
              , [ "v05" , "v00" ]
              , [ "v10" , "v00" ]
              ]
          )
        ]

    it "closure param & result ; context insenisitve return value mix" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            let
              clo1_ap = \[] p10 p11 ->
                letS
                  v10 = p10 $ p11
                v10
              clo2_id = \[] p20 ->
                p20
            letS
              v00 = #T_Int64 0
              v01 = clo1_ap $
              v02 = v01 $ v01
              v03 = clo2_id $
              v04 = v02 $ v03
              v05 = v04 $ v00
            v05
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["p11", "lit:T_Int64"]
              , ["p20", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v04", "lit:T_Int64"]
              , ["v05", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ [ "p10" , "v00" ]
              , [ "p11" , "v00" ]
              , [ "p20" , "v00" ]
              , [ "v00" , "v00" ]
              , [ "v04" , "v00" ]
              , [ "v05" , "v00" ]
              , [ "v10" , "v00" ]
              ]
          )
        ]

  ----------------------------

  describe "CBy - lit - unknown - oversaturated" $ do

    it "fun param & result ; simple" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun1_id $
              v02 = v01 $ v01 v01 v00
            v02
          fun1_id p10 =
            p10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["v00", "v00"]
              , ["v02", "v00"]
              ]
          )
        ]

    it "fun param & result ; ap id id lit" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun1_ap $
              v02 = fun2_id $
              v03 = v01 $ v02 v02 v00 -- ap id id lit
            v03
          fun1_ap p10 p11 =
            letS
              v10 = p10 $ p11
            v10
          fun2_id p20 =
            p20
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p20", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v03", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p20", "v00"]
              , ["v00", "v00"]
              , ["v03", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    it "fun param & result ; ap ap' id lit + BUGFIX" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun1_ap $
              v02 = fun2_ap $
              v03 = fun3_id $
              v04 = v01 $ v02 v03 v00 -- ap ap' id lit
            v04
          fun1_ap p10 p11 =
            letS
              v10 = p10 $ p11
            v10
          fun2_ap p20 p21 =
            letS
              v20 = p20 $ p21
            v20
          fun3_id p30 =
            p30
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p21", "lit:T_Int64"]
              , ["p30", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v04", "lit:T_Int64"]
              , ["v20", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p21", "v00"]
              , ["p30", "v00"]
              , ["v00", "v00"]
              , ["v04", "v00"]
              , ["v20", "v00"]
              ]
          )
        ]
      -- BUG: v04 must not have PNode [fixed]
      -- CFA ApplyChain BUGFIX test
      filterAndSort ["PNode", "ApplyChain"] cfa `sameAs` Map.fromList
        [ ( "ApplyChain" , [ [ "normal-call" , "v04" , "fun1_ap" , "0" , "2" , "3" ] ] )
        , ( "PNode",
              [ [ "p10" , "fun2_ap" , "2" , "2" ]
              , [ "p11" , "fun3_id" , "1" , "1" ]
              , [ "p20" , "fun3_id" , "1" , "1" ]
              , [ "v01" , "fun1_ap" , "2" , "2" ]
              , [ "v02" , "fun2_ap" , "2" , "2" ]
              , [ "v03" , "fun3_id" , "1" , "1" ]
              , [ "v10" , "fun2_ap" , "2" , "1" ]
              ]
          )
        ]

    it "PNode propagation ; direct" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            let
              c00 = \[] p00 p01 ->
                letS
                  v10 = [Tup2 p00 p01]
                v10
            letS
              v00 = #T_Int64 0
              v01 = [Tup0]
              v02 = c00 $ -- NOTE: no PNode for c00 because it is not used as a value anywhere but the call target
              v03 = v02 $ v00
              v04 = v03 $
              v05 = v04 $ v01
            v05
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p00", "lit:T_Int64"]
              , ["p01", "Tup0"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "Tup0"]
              , ["v05", "Tup2"]
              , ["v10", "Tup2"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p00", "v00"]
              , ["p01", "v01"]
              , ["v00", "v00"]
              , ["v01", "v01"]
              , ["v05", "v10"]
              , ["v10", "v10"]
              ]
          )
        ]
      filterAndSort ["PNode", "ApplyChain"] cfa `sameAs` Map.fromList
        [ ( "ApplyChain" , [] )
        , ( "PNode",
              [ [ "v02" , "c00" , "2" , "2" ]
              , [ "v03" , "c00" , "2" , "1" ]
              , [ "v04" , "c00" , "2" , "1" ]
              ]
          )
        ]

    it "PNode propagation ; indirect" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            let
              c00 = \[] p00 p01 ->
                letS
                  v10 = [Tup2 p00 p01]
                v10
            letS
              v00 = #T_Int64 0
              v01 = [Tup0]
              c01 = c00 -- NOTE: creates PNode for c00 because it is used as a value
              v02 = c01 $
              v03 = v02 $ v00
              v04 = v03 $
              v05 = v04 $ v01
            v05
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p00", "lit:T_Int64"]
              , ["p01", "Tup0"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "Tup0"]
              , ["v05", "Tup2"]
              , ["v10", "Tup2"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p00", "v00"]
              , ["p01", "v01"]
              , ["v00", "v00"]
              , ["v01", "v01"]
              , ["v05", "v10"]
              , ["v10", "v10"]
              ]
          )
        ]
      filterAndSort ["PNode", "ApplyChain"] cfa `sameAs` Map.fromList
        [ ( "ApplyChain" , [] )
        , ( "PNode",
              [ [ "c00" , "c00" , "2" , "2" ]
              , [ "c01" , "c00" , "2" , "2" ]
              , [ "v02" , "c00" , "2" , "2" ]
              , [ "v03" , "c00" , "2" , "1" ]
              , [ "v04" , "c00" , "2" , "1" ]
              ]
          )
        ]

    it "fun param & result ; ap ap id lit" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun1_ap $
              v02 = fun2_id $
              v03 = v01 $ v01 v02 v00 -- ap ap id lit
            v03
          fun1_ap p10 p11 =
            letS
              v10 = p10 $ p11 -- NOTE: ap ap results PointsTo("p10", "p11")
            v10
          fun2_id p20 =
            p20
        |]
      addUsedM cfa

      -- NOTE: ap ap results PointsTo("p10", "p11")
      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]  -- due to PointsTo("p10", "p11")
              , ["p11", "lit:T_Int64"]
              , ["p20", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v03", "lit:T_Int64"]
              , ["v10", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]  -- due to PointsTo("p10", "p11")
              , ["p11", "v00"]
              , ["p20", "v00"]
              , ["v00", "v00"]
              , ["v03", "v00"]
              , ["v10", "v00"]
              ]
          )
        ]

    it "clo param & result ; ap ap' id lit + BUGFIX" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = clo_fun1_ap $
              v02 = clo_fun2_ap $
              v03 = clo_fun3_id $
              v04 = v01 $ v02 v03 v00 -- ap ap' id lit
            v04
          clo_fun1_ap =
            let
              fun1_ap = \[] p10 p11 ->
                letS
                  v10 = p10 $ p11
                v10
            fun1_ap
          clo_fun2_ap =
            let
              fun2_ap = \[] p20 p21 ->
                letS
                  v20 = p20 $ p21
                v20
            fun2_ap
          clo_fun3_id =
            let
              fun3_id = \[] p30 ->
                p30
            fun3_id
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p21", "lit:T_Int64"]
              , ["p30", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v04", "lit:T_Int64"]
              , ["v20", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p21", "v00"]
              , ["p30", "v00"]
              , ["v00", "v00"]
              , ["v04", "v00"]
              , ["v20", "v00"]
              ]
          )
        ]
      -- BUG: v04 must not have PNode [fixed]
      -- CFA ApplyChain BUGFIX test
      filterAndSort ["PNode", "ApplyChain"] cfa `sameAs` Map.fromList
        [ ( "ApplyChain" , [ [ "normal-call" , "v04" , "fun1_ap" , "0" , "2" , "3" ] ] )
        , ( "PNode",
              [ [ "fun1_ap" , "fun1_ap" , "2" , "2" ]
              , [ "fun2_ap" , "fun2_ap" , "2" , "2" ]
              , [ "fun3_id" , "fun3_id" , "1" , "1" ]
              , [ "p10" , "fun2_ap" , "2" , "2" ]
              , [ "p11" , "fun3_id" , "1" , "1" ]
              , [ "p20" , "fun3_id" , "1" , "1" ]
              , [ "v01" , "fun1_ap" , "2" , "2" ]
              , [ "v02" , "fun2_ap" , "2" , "2" ]
              , [ "v03" , "fun3_id" , "1" , "1" ]
              , [ "v10" , "fun2_ap" , "2" , "1" ]
              ]
          )
        ]

  ----------------------------

  describe "CBy - lit - known - oversaturated" $ do

    it "fun param & result ; simple" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v02 = fun1_id $ fun1_id fun1_id v00
            v02
          fun1_id p10 =
            p10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["v00", "v00"]
              , ["v02", "v00"]
              ]
          )
        ]

    it "fun param & result ; ap ap' id lit + BUGFIX" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v04 = fun1_ap $ fun2_ap fun3_id v00 -- ap ap' id lit
            v04
          fun1_ap p10 p11 =
            letS
              v10 = p10 $ p11
            v10
          fun2_ap p20 p21 =
            letS
              v20 = p20 $ p21
            v20
          fun3_id p30 =
            p30
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p21", "lit:T_Int64"]
              , ["p30", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v04", "lit:T_Int64"]
              , ["v20", "lit:T_Int64"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p21", "v00"]
              , ["p30", "v00"]
              , ["v00", "v00"]
              , ["v04", "v00"]
              , ["v20", "v00"]
              ]
          )
        ]
      -- BUG: v04 must not have PNode [fixed]
      -- CFA ApplyChain BUGFIX test
      filterAndSort ["PNode", "ApplyChain"] cfa `sameAs` Map.fromList
        [ ( "ApplyChain" , [ [ "normal-call" , "v04" , "fun1_ap" , "0" , "2" , "3" ] ] )
        , ( "PNode",
              [ [ "fun2_ap" , "fun2_ap" , "2" , "2" ]
              , [ "fun3_id" , "fun3_id" , "1" , "1" ]
              , [ "p10" , "fun2_ap" , "2" , "2" ]
              , [ "p11" , "fun3_id" , "1" , "1" ]
              , [ "p20" , "fun3_id" , "1" , "1" ]
              , [ "v10" , "fun2_ap" , "2" , "1" ]
              ]
          )
        ]

  describe "CFA - coverage" $ do

    {-
    "CFA-05"  undersaturated known call: create PNode ; pass arguments
    "CFA-11"  undersaturated PNode call: create new PNode ; copy PNode arguments
    -}
    it "node - known - fun param & result ; gradually saturated call " $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = #T_Int64 0
              v02 = #T_Int64 0
              v03 = fun1_tup3 $ v00
              v04 = v03 $ v01
              v05 = v04 $ v02
            v05
          fun1_tup3 p10 p11 p12 =
            letS
              v10 = [Tup3 p10 p11 p12]
            v10
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["p11", "lit:T_Int64"]
              , ["p12", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              , ["v05", "Tup3"]
              , ["v10", "Tup3"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["p11", "v01"]
              , ["p12", "v02"]
              , ["v00", "v00"]
              , ["v01", "v01"]
              , ["v02", "v02"]
              , ["v05", "v10"]
              , ["v10", "v10"]
              ]
          )
        ]

    {-
    "CFA-15"  oversaturated PNode call
    "CFA-24"  oversaturated apply chain link
    -}
    it "node - known - fun param & result ; oversaturated non-empty PNode" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = #T_Int64 0
              v02 = #T_Int64 0
              v03 = fun1_tup2 $ v00
              v04 = v03 $ v01 fun3_id v02
            v04
          fun1_tup2 p10 p11 =
            letS
              v10 = [Tup2 p10 p11]
            let
              v11 = \[] p20 p21 ->
                p21
            letS
              v12 = v11 $ v10
            v12
          fun3_id p30 =
            p30
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["p11", "lit:T_Int64"]
              , ["p20", "Tup2"]
              , ["p30", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              , ["v04", "lit:T_Int64"]
              , ["v10", "Tup2"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["p11", "v01"]
              , ["p20", "v10"]
              , ["p30", "v02"]
              , ["v00", "v00"]
              , ["v01", "v01"]
              , ["v02", "v02"]
              , ["v04", "v02"]
              , ["v10", "v10"]
              ]
          )
        ]

    {-
    "CFA-20"  undersaturated apply chain link
    "CFA-21"  undersaturated apply chain link
    "CFA-22"  undersaturated apply chain link
    -}
    it "node - known - fun param & result ; undersaturated gradually built non-empty PNode & apply chain" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = #T_Int64 0
              v02 = #T_Int64 0
              v03 = fun1_tup5 $ v00
              v04 = fun2_id $ v03 v01 v02
              v05 = v04 $ v00 v01
            v05
          fun1_tup5 p10 p11 p12 p13 p14 =
            letS
              v10 = [Tup5 p10 p11 p12 p13 p14]
            v10
          fun2_id p30 =
            p30
        |]
      addUsedM cfa

      toCBy cfa `sameAs` Map.fromList
        [ ( "ExternalOrigin", [] )
        , ( "TagValue",
              [ ["p10", "lit:T_Int64"]
              , ["p11", "lit:T_Int64"]
              , ["p12", "lit:T_Int64"]
              , ["p13", "lit:T_Int64"]
              , ["p14", "lit:T_Int64"]
              , ["v00", "lit:T_Int64"]
              , ["v01", "lit:T_Int64"]
              , ["v02", "lit:T_Int64"]
              , ["v05", "Tup5"]
              , ["v10", "Tup5"]
              ]
          )
        , ( "NodeOrigin",
              [ ["p10", "v00"]
              , ["p11", "v01"]
              , ["p12", "v02"]
              , ["p13", "v00"]
              , ["p14", "v01"]
              , ["v00", "v00"]
              , ["v01", "v01"]
              , ["v02", "v02"]
              , ["v05", "v10"]
              , ["v10", "v10"]
              ]
          )
        ]

  -- undersaturated
  {-
    fun param
    closure param
      X
    known
    unknown

    thunk
    apply chain
    partially applied result
  -}

{-
  Missing:
  "CBy-1"   calling a constructor / forcing
  "CBy-3"   external
  "CBy-6"   external alt value
  "CBy-7"   node pattern match
  "CBy-8"   external pattern match

-}
  describe "CFA - coverage" $ do
    pure ()

  describe "Print" $ do
    it "Print" $ do
      printUsedM


{-
CFA
  .decl ApplyChain(v:Variable, f:CodeName, step:number, consumed:number, arg_count:number)
  .decl PNode(v:Variable, fun:CodeName, arity:number, remained:number)
  .decl PNodeArgument(v:Variable, fun:CodeName, i:number, value:CodeName)
  .decl Called(instruction:Variable, f:CodeName)

MISC
  .decl CallPNode1(inst:Variable, pnode:Variable, last_arg:Variable)
  .decl CallPNode2(inst:Variable, pnode:Variable, before_last_arg:Variable, last_arg:Variable)

  .decl PointsTo(src:Variable, dst:Variable)
  .decl TypeVarPointsTo(result:Variable, ty_var:Variable, dst:Variable)

LVA
  .decl ReachableCode(n:CodeName, msg:symbol)
  .decl Reachable(inst:Variable)
  .decl DeadCode(n:CodeName)

  .decl DeadExternal(n:External)
  .decl ReachableExternal(n:External)

CBy
  .decl NodeOrigin(v:Variable, value:CodeName)
  .decl ExternalOrigin(v:Variable, ext_result:Variable, ty_var:Variable)
  .decl TagValue(v:Variable, t:Tag)

AST
  .decl HasInst(n:CodeName, v:Variable)
  .decl RetTup1Node0(ext:External, ty_node:Variable)
  .decl RetTup(ext:External, tag:Tag, i:number, ty_node:Variable)

Check
  .decl Error(v:Variable, msg:symbol)
  .decl MissingValue(v:Variable)

PrimOp
  .decl Array(ext_result:Variable, ty_node:Variable, item:Variable)

  .decl MVar(ext_result:Variable, ty_node:Variable, item:Variable)
  .decl MVarDef(ext_result:Variable, ty_node:Variable)

  .decl RaisedEx(f:Variable)

  .decl TVar(ext_result:Variable, ty_node:Variable, item:Variable)

  .decl Spark(item:Variable)

  .decl WeakPtr(ext_result:Variable, ty_node:Variable, item:Variable)
  .decl WeakFinalizer(ext_result:Variable, finalizer:Variable)

  .decl MutVar(ext_result:Variable, ty_node:Variable, item:Variable)

  .decl StablePtr(ext_result:Variable, ty_node:Variable, item:Variable)
-}
