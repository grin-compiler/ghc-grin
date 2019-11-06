{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module CFASpec where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Test.Hspec
import Test.QuickCheck
import System.IO

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
