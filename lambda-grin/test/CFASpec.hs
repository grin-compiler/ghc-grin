{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module CFASpec where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck
import System.IO
import Text.Show.Pretty (pPrint,ppShow)
import Text.PrettyPrint.ANSI.Leijen

import Lambda.TH
import Lambda.ControlFlowAnalysisM
import Grin.Pretty (PP(..))

runTests :: IO ()
runTests = hspec spec

{-
  TODO:
    test each datalog rule
-}

spec :: Spec
spec = do
{-
  describe "simple" $ do
    it "case" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
        main =
          letS
            p0 = #T_Int64 0
            v0 = fun $ p0
          v0

        fun p =
          letS
            x = case p of
              _ @ alt.1 ->
                letS
                  r = #T_Int64 1
                r
          x
        |]

      cfa Map.! "Called" `shouldBe` [["v0","fun"]]
      cfa Map.! "ReachableCode" `shouldBe`
        [ ["alt.1","ALT:DEFAULT"]
        , ["fun","CALL,v0"]
        , ["main","INITIAL"]
        ]
-}
  ----------------------------

  let filterAndSort keys m = fmap sort $ Map.restrictKeys m (Set.fromList keys)

      sameAs :: Show a => a -> a -> IO ()
      sameAs a b = (PP (ppShow a)) `shouldBe` (PP (ppShow b))

      toCBy = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue"]

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
              v20 = fun1 $ -- CALL: does nothing
            v20
          fun1 p10 =
            letS
              v10 = p10
            v10
        |]

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

{-
  ----------------------------

  describe "CBy - lit - known - undersaturated" $ do

    it "fun param" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          main =
            letS
              v00 = #T_Int64 0
              v01 = fun1 $ v00
              v02 = v01 $ v00
            v02
          fun1 p10 p11 =
            letS
              v10 = p10
              v11 = #T_Int64 1
            v11
        |]
      pure ()
-}
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
