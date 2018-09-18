{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module TestUtils
  ( copyPathRecursive
  , createSymbolicLink
  , modifyPermissions
  , tryCreateSymbolicLink
  ) where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory
import System.FilePath ((</>))
#ifdef mingw32_HOST_OS
import System.FilePath (takeDirectory)
import qualified System.Win32 as Win32
#else
import System.Posix (createSymbolicLink)
#endif

#ifdef mingw32_HOST_OS
# if defined i386_HOST_ARCH
#  define WINAPI stdcall
# elif defined x86_64_HOST_ARCH
#  define WINAPI ccall
# else
#  error unknown architecture
# endif
foreign import WINAPI unsafe "windows.h CreateSymbolicLinkW"
  c_CreateSymbolicLink :: Ptr CWchar -> Ptr CWchar -> CULong -> IO CUChar
#endif

-- | @'copyPathRecursive' path@ copies an existing file or directory at
--   /path/ together with its contents and subdirectories.
--
--   Warning: mostly untested and might not handle symlinks correctly.
copyPathRecursive :: FilePath -> FilePath -> IO ()
copyPathRecursive source dest =
  (`ioeSetLocation` "copyPathRecursive") `modifyIOError` do
    dirExists <- doesDirectoryExist source
    if dirExists
      then do
        contents <- listDirectory source
        createDirectory dest
        mapM_ (uncurry copyPathRecursive)
          [(source </> x, dest </> x) | x <- contents]
      else copyFile source dest

modifyPermissions :: FilePath -> (Permissions -> Permissions) -> IO ()
modifyPermissions path modify = do
  permissions <- getPermissions path
  setPermissions path (modify permissions)

#ifdef mingw32_HOST_OS
createSymbolicLink :: String -> String -> IO ()
createSymbolicLink target link =
  (`ioeSetLocation` "createSymbolicLink") `modifyIOError` do
    isDir <- (fromIntegral . fromEnum) `fmap`
             doesDirectoryExist (takeDirectory link </> target)
    let target' = fixSlash <$> target
    withCWString target' $ \ pTarget ->
      withCWString link $ \ pLink -> do
        status <- c_CreateSymbolicLink pLink pTarget isDir
        if status == 0
          then do
            errCode <- Win32.getLastError
            if errCode == c_ERROR_PRIVILEGE_NOT_HELD
              then ioError . (`ioeSetErrorString` permissionErrorMsg) $
                   mkIOError permissionErrorType "" Nothing (Just link)
              else Win32.failWith "createSymbolicLink" errCode
          else return ()
  where c_ERROR_PRIVILEGE_NOT_HELD = 0x522
        permissionErrorMsg = "no permission to create symbolic links"
        fixSlash '/' = '\\'
        fixSlash c   = c
#endif

-- | Attempt to create a symbolic link.  On Windows, this falls back to
--   copying if forbidden due to Group Policies.
tryCreateSymbolicLink :: FilePath -> FilePath -> IO ()
tryCreateSymbolicLink target link = createSymbolicLink target link
#ifdef mingw32_HOST_OS
  `catchIOError` \ e ->
    if isPermissionError e
    then copyPathRecursive (takeDirectory link </> target) link
    else ioError e
#endif
