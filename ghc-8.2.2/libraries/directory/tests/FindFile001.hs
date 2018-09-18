{-# LANGUAGE CPP #-}
module FindFile001 where
#include "util.inl"
import System.FilePath ((</>))

main :: TestEnv -> IO ()
main _t = do
  writeFile "foo" ""
  found <- findFile ("." : undefined) "foo"
  T(expectEq) () found (Just ("." </> "foo"))
