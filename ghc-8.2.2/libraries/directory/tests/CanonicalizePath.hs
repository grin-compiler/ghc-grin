{-# LANGUAGE CPP #-}
module CanonicalizePath where
#include "util.inl"
import System.FilePath ((</>), dropFileName, dropTrailingPathSeparator,
                        normalise, takeFileName)
import TestUtils
#ifdef mingw32_HOST_OS
import System.Directory.Internal (win32_getFinalPathNameByHandle)
import qualified System.Win32 as Win32
#endif

main :: TestEnv -> IO ()
main _t = do
  dot <- canonicalizePath ""
  dot2 <- canonicalizePath "."
  dot3 <- canonicalizePath "./"
  dot4 <- canonicalizePath "./."
  T(expectEq) () dot (dropTrailingPathSeparator dot)
  T(expectEq) () dot dot2
  T(expectEq) () dot dot3
  T(expectEq) () dot dot4

  writeFile "bar" ""
  bar <- canonicalizePath "bar"
  bar2 <- canonicalizePath "bar/"
  bar3 <- canonicalizePath "bar/."
  bar4 <- canonicalizePath "bar/./"
  bar5 <- canonicalizePath "./bar"
  bar6 <- canonicalizePath "./bar/"
  bar7 <- canonicalizePath "./bar/."
  T(expectEq) () bar (normalise (dot </> "bar"))
  T(expectEq) () bar bar2
  T(expectEq) () bar bar3
  T(expectEq) () bar bar4
  T(expectEq) () bar bar5
  T(expectEq) () bar bar6
  T(expectEq) () bar bar7

  createDirectory "foo"
  foo <- canonicalizePath "foo"
  foo2 <- canonicalizePath "foo/"
  foo3 <- canonicalizePath "foo/."
  foo4 <- canonicalizePath "foo/./"
  foo5 <- canonicalizePath "./foo"
  foo6 <- canonicalizePath "./foo/"
  T(expectEq) () foo (normalise (dot </> "foo"))
  T(expectEq) () foo foo2
  T(expectEq) () foo foo3
  T(expectEq) () foo foo4
  T(expectEq) () foo foo5
  T(expectEq) () foo foo6

  -- should not fail for non-existent paths
  fooNon <- canonicalizePath "foo/non-existent"
  fooNon2 <- canonicalizePath "foo/non-existent/"
  fooNon3 <- canonicalizePath "foo/non-existent/."
  fooNon4 <- canonicalizePath "foo/non-existent/./"
  fooNon5 <- canonicalizePath "./foo/non-existent"
  fooNon6 <- canonicalizePath "./foo/non-existent/"
  fooNon7 <- canonicalizePath "./foo/./non-existent"
  fooNon8 <- canonicalizePath "./foo/./non-existent/"
  T(expectEq) () fooNon (normalise (foo </> "non-existent"))
  T(expectEq) () fooNon fooNon2
  T(expectEq) () fooNon fooNon3
  T(expectEq) () fooNon fooNon4
  T(expectEq) () fooNon fooNon5
  T(expectEq) () fooNon fooNon6
  T(expectEq) () fooNon fooNon7
  T(expectEq) () fooNon fooNon8

  supportsSymbolicLinks <- do
#ifdef mingw32_HOST_OS
    hasSymbolicLinkPrivileges <-
      (True <$ createSymbolicLink "_symlinktest_src" "_symlinktest_dst")
        -- only test if symbolic links can be created
        -- (usually disabled on Windows by group policy)
        `catchIOError` \ e ->
          if isPermissionError e
          then pure False
          else ioError e

    supportsGetFinalPathNameByHandle <-
      (True <$ win32_getFinalPathNameByHandle Win32.nullHANDLE 0)
        `catchIOError` \ e ->
          case ioeGetErrorType e of
            UnsupportedOperation -> pure False
            _ -> pure True

    pure (hasSymbolicLinkPrivileges && supportsGetFinalPathNameByHandle)
#else
    pure True
#endif

  when supportsSymbolicLinks $ do

    let barQux = dot </> "bar" </> "qux"

    createSymbolicLink "../bar" "foo/bar"
    T(expectEq) () bar =<< canonicalizePath "foo/bar"
    T(expectEq) () barQux =<< canonicalizePath "foo/bar/qux"

    createSymbolicLink "foo" "lfoo"
    T(expectEq) () foo =<< canonicalizePath "lfoo"
    T(expectEq) () foo =<< canonicalizePath "lfoo/"
    T(expectEq) () bar =<< canonicalizePath "lfoo/bar"
    T(expectEq) () barQux =<< canonicalizePath "lfoo/bar/qux"

    -- FIXME: uncomment this test once #64 is fixed
    -- createSymbolicLink "../foo/non-existent" "foo/qux"
    -- qux <- canonicalizePath "foo/qux"
    -- T(expectEq) () qux (dot </> "../foo/non-existent")

  caseInsensitive <-
    (False <$ createDirectory "FOO")
      `catch` \ e ->
        if isAlreadyExistsError e
        then pure True
        else throwIO e

  -- if platform is case-insensitive, we expect case to be canonicalized too
  when caseInsensitive $ do
    foo7 <- canonicalizePath "FOO"
    foo8 <- canonicalizePath "FOO/"
    T(expectEq) () foo foo7
    T(expectEq) () foo foo8

    fooNon9 <- canonicalizePath "FOO/non-existent"
    fooNon10 <- canonicalizePath "fOo/non-existent/"
    fooNon11 <- canonicalizePath "foO/non-existent/."
    fooNon12 <- canonicalizePath "FoO/non-existent/./"
    fooNon13 <- canonicalizePath "./fOO/non-existent"
    fooNon14 <- canonicalizePath "./FOo/non-existent/"
    cfooNon15 <- canonicalizePath "./FOO/./NON-EXISTENT"
    cfooNon16 <- canonicalizePath "./FOO/./NON-EXISTENT/"
    T(expectEq) () fooNon fooNon9
    T(expectEq) () fooNon fooNon10
    T(expectEq) () fooNon fooNon11
    T(expectEq) () fooNon fooNon12
    T(expectEq) () fooNon fooNon13
    T(expectEq) () fooNon fooNon14
    T(expectEq) () fooNon (dropFileName cfooNon15 <>
                           (toLower <$> takeFileName cfooNon15))
    T(expectEq) () fooNon (dropFileName cfooNon16 <>
                           (toLower <$> takeFileName cfooNon16))
    T(expectNe) () fooNon cfooNon15
    T(expectNe) () fooNon cfooNon16

    setCurrentDirectory "foo"
    foo9 <- canonicalizePath "../FOO"
    foo10 <- canonicalizePath "../FOO/"
    T(expectEq) () foo foo9
    T(expectEq) () foo foo10

    -- Make sure long file names can be canonicalized too
    -- (i.e. GetLongPathName by itself won't work)
    createDirectory "verylongdirectoryname"
    vldn <- canonicalizePath "verylongdirectoryname"
    vldn2 <- canonicalizePath "VERYLONGDIRECTORYNAME"
    T(expectEq) () vldn vldn2
