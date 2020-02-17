{-# LANGUAGE CPP #-}
module Xdg where
#if MIN_VERSION_base(4, 7, 0)
import qualified Data.List as List
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>), searchPathSeparator)
#endif
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  -- smoke tests
  _ <- getXdgDirectoryList XdgDataDirs
  _ <- getXdgDirectoryList XdgConfigDirs

  T(expect) () True -- avoid warnings about redundant imports

  -- setEnv, unsetEnv require base 4.7.0.0+
#if MIN_VERSION_base(4, 7, 0)
#if !defined(mingw32_HOST_OS)
  unsetEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  T(expectEq) () (home </> ".config/mow") =<< getXdgDirectory XdgConfig "mow"
#endif

  setEnv "XDG_DATA_HOME"   "ar"
  setEnv "XDG_CONFIG_HOME" "aw"
  setEnv "XDG_CACHE_HOME"  "ba"
  T(expectEq) () ("ar" </> "ff") =<< getXdgDirectory XdgData   "ff"
  T(expectEq) () ("aw" </> "oo") =<< getXdgDirectory XdgConfig "oo"
  T(expectEq) () ("ba" </> "rk") =<< getXdgDirectory XdgCache  "rk"

  unsetEnv "XDG_CONFIG_DIRS"
  unsetEnv "XDG_DATA_DIRS"
  _xdgConfigDirs <- getXdgDirectoryList XdgConfigDirs
  _xdgDataDirs <- getXdgDirectoryList XdgDataDirs

#if !defined(mingw32_HOST_OS)
  T(expectEq) () ["/etc/xdg"] _xdgConfigDirs
  T(expectEq) () ["/usr/local/share/", "/usr/share/"] _xdgDataDirs
#endif

  setEnv "XDG_DATA_DIRS" (List.intercalate [searchPathSeparator] ["/a", "/b"])
  setEnv "XDG_CONFIG_DIRS" (List.intercalate [searchPathSeparator] ["/c", "/d"])
  T(expectEq) () ["/a", "/b"] =<< getXdgDirectoryList XdgDataDirs
  T(expectEq) () ["/c", "/d"] =<< getXdgDirectoryList XdgConfigDirs
#endif

  return ()
