{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpCCSSpec where

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

      toCCSOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------

  describe "GHC CCS PrimOps" $ do

    it "clearCCS#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog|
          primop effectful
            "clearCCS#" :: (tf.4 : {"State#" {RealWorld} @ t.14} @ t.13 -> {"GHC.Prim.Unit#" %a.1} @ t.15) -> {"State#" {RealWorld} @ t.17} @ t.16 -> {"GHC.Prim.Unit#" %a.1} @ t.18

          main =
            letS
              v00 = #T_Token "RealWorld"
              v01 = "clearCCS#" $ fun1 v00
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
      toCCSOp cfa `sameAs` Map.fromList
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
