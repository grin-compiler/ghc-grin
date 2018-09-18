{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
#include <HsDirectoryConfig.h>

module System.Directory.Internal
  ( module System.Directory.Internal.Config

#ifdef HAVE_UTIMENSAT
  , module System.Directory.Internal.C_utimensat
#endif

#ifdef mingw32_HOST_OS
  , module System.Directory.Internal.Windows
#else
  , module System.Directory.Internal.Posix
#endif

  ) where
import System.Directory.Internal.Config

#ifdef HAVE_UTIMENSAT
import System.Directory.Internal.C_utimensat
#endif

#ifdef mingw32_HOST_OS
import System.Directory.Internal.Windows
#else
import System.Directory.Internal.Posix
#endif
