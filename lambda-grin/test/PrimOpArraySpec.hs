{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module PrimOpArraySpec where

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

      toArrOp = filterAndSort ["NodeOrigin", "ExternalOrigin", "TagValue", "Array"]
      addUsedM a = modifyIORef usedRules (\x -> mappend x . Set.fromList . head . Map.elems . filterAndSort ["Used"] $ a)
      printUsedM = readIORef usedRules >>= pPrint

  ----------------------------
{-
  primop effectful
    + "newArray#" :: T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}

  primop pure
    - "sameMutableArray#" :: {"MutableArray#" %s %a} -> {"MutableArray#" %s %a} -> T_Int64

  primop effectful
    + "readArray#"  :: {"MutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    + "writeArray#" :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    - "sizeofArray#"        :: {"Array#" %a} -> T_Int64
    - "sizeofMutableArray#" :: {"MutableArray#" %s %a} -> T_Int64
    + "indexArray#"         :: {"Array#" %a} -> T_Int64 -> {"GHC.Prim.Unit#" %a}

  primop effectful
    + "unsafeFreezeArray#" :: {"MutableArray#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"Array#" %a}}
    + "unsafeThawArray#"   :: {"Array#" %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    + "copyArray#"         :: {"Array#" %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "copyMutableArray#"  :: {"MutableArray#" %s %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "cloneArray#"        :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"Array#" %a}
    + "cloneMutableArray#" :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    + "freezeArray#"       :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"Array#" %a}}
    + "thawArray#"         :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    + "casArray#"          :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}

  Small Arrays

  primop effectful
    + "newSmallArray#" :: T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}

  primop pure
    - "sameSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> {"SmallMutableArray#" %s %a} -> T_Int64

  primop effectful
    + "readSmallArray#"  :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    + "writeSmallArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    - "sizeofSmallArray#"        :: {"SmallArray#" %a} -> T_Int64
    - "sizeofSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64
    + "indexSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> {"GHC.Prim.Unit#" %a}

  primop effectful
    + "unsafeFreezeSmallArray#" :: {"SmallMutableArray#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallArray#" %a}}
    + "unsafeThawSmallArray#"   :: {"SmallArray#" %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    + "copySmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "copySmallMutableArray#"  :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    + "cloneSmallArray#"        :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"SmallArray#" %a}
    + "cloneSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    + "freezeSmallArray#"       :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallArray#" %a}}
    + "thawSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    + "casSmallArray#"          :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}
-}
  describe "GHC Array PrimOps" $ do

    it "newArray#" $ do
      cfa <- controlFlowAnalysisM ["main"] [prog2|
          primop effectful
            "newArray#" :: (T_Int64) @ t.106 -> %a.6 -> {"State#" %s.2} @ t.107 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.2 %a.6} @ t.109} @ t.108
          main =
            letS
              v00 = #T_Int64 10
              v01 = [Tup0]
              v02 = #T_Token "RealWorld"
              v03 = "newArray#" $ v00 v01 v02
              v05 = case v03 of
                ("GHC.Prim.Unit#" v04) @ a00 ->
                  v04
            v05
        |]
      addUsedM cfa
      pPrint $ toArrOp cfa
{-
      toArrOp cfa `sameAs` Map.fromList
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
-}

  describe "Coverage" $ do
    it "Used Rules" $ do
      printUsedM
