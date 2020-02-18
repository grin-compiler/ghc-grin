{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module ParserSpec where

import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck

import Lambda.TH
import Lambda.Pretty
import Lambda.Parse
import Lambda.Syntax
import Grin.Pretty (PP(..))

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "simple" $ do
    it "case" $ do
      let before = [prog|
        test p =
          letS
            x = case p of
              _ @ alt.1 ->
                letS
                  r = #T_Int64 1
                r
          x
        |]
      let after = Program [] []
            [ Def "test" ["p"]
              ( LetS
                [ ("x", Case "p"
                    [ Alt "alt.1" DefaultPat
                        ( LetS [("r", Lit (LInt64 1)) ] (Var "r"))
                    ]
                  )
                ] (Var "x")
              )
            ]
      (PP before) `shouldBe` (PP after)
