{-# LANGUAGE CPP #-}
module MakeAbsolute where
#include "util.inl"
import System.FilePath ((</>), addTrailingPathSeparator,
                        dropTrailingPathSeparator, normalise)

main :: TestEnv -> IO ()
main _t = do
  dot <- makeAbsolute ""
  dot2 <- makeAbsolute "."
  dot3 <- makeAbsolute "./."
  T(expectEq) () dot (dropTrailingPathSeparator dot)
  T(expectEq) () dot dot2
  T(expectEq) () dot dot3

  sdot <- makeAbsolute "./"
  sdot2 <- makeAbsolute "././"
  T(expectEq) () sdot (addTrailingPathSeparator sdot)
  T(expectEq) () sdot sdot2

  foo <- makeAbsolute "foo"
  foo2 <- makeAbsolute "foo/."
  foo3 <- makeAbsolute "./foo"
  T(expectEq) () foo (normalise (dot </> "foo"))
  T(expectEq) () foo foo2
  T(expectEq) () foo foo3

  sfoo <- makeAbsolute "foo/"
  sfoo2 <- makeAbsolute "foo/./"
  sfoo3 <- makeAbsolute "./foo/"
  T(expectEq) () sfoo (normalise (dot </> "foo/"))
  T(expectEq) () sfoo sfoo2
  T(expectEq) () sfoo sfoo3
