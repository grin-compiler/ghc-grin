{-# LANGUAGE CPP #-}
module PathIsSymbolicLink where
#include "util.inl"
import TestUtils

main :: TestEnv -> IO ()
main _t = do
  success <- (createSymbolicLink "x" "y" >> return True)
#ifdef mingw32_HOST_OS
    -- only test if symbolic links can be created
    -- (usually disabled on Windows by group policy)
    `catchIOError` \ e ->
      if isPermissionError e
      then return False
      else ioError e
#endif
  when success $
    T(expect) () =<< pathIsSymbolicLink "y"
