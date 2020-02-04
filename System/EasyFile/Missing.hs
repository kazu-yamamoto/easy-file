{-# LANGUAGE CPP #-}

module System.EasyFile.Missing where

----------------------------------------------------------------

import Data.Time
import Data.Time.Clock.POSIX
import Data.Word (Word64)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Control.Exception
import System.Win32.File
import System.Win32.Time
import System.Win32.Types (HANDLE)
#else
import System.Posix.Files
import System.Posix.Types
#endif

----------------------------------------------------------------

{-|
  This function tells whether or not a file\/directory is symbolic
  link.
-}
isSymlink :: FilePath -> IO Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isSymlink _ = return False
#else
isSymlink file = isSymbolicLink <$> getSymbolicLinkStatus file
#endif

{-|
  This function returns the link counter of a file\/directory.
-}
getLinkCount :: FilePath -> IO (Maybe Int)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getLinkCount _ = return Nothing
#else
getLinkCount file = Just . fromIntegral . linkCount <$> getFileStatus file
#endif

{-|
  This function returns whether or not a directory has sub-directories.
-}
hasSubDirectories :: FilePath -> IO (Maybe Bool)
#ifdef darwin_HOST_OS
hasSubDirectories _ = return Nothing
#else
hasSubDirectories file = do
  Just n <- getLinkCount file
  return $ Just (n > 2)
#endif

----------------------------------------------------------------

{-|
The 'getCreationTime' operation returns the
UTC time at which the file or directory was created.
The time is only available on Windows.
-}
getCreationTime :: FilePath -> IO (Maybe UTCTime)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getCreationTime file = Just . creationTime <$> fileTime file
#else
getCreationTime _ = return Nothing
#endif

{-|
The 'getChangeTime' operation returns the
UTC time at which the file or directory was changed.
The time is only available on Unix and Mac.
Note that Unix's rename() does not change ctime but
MacOS's rename() does.
-}
getChangeTime :: FilePath -> IO (Maybe UTCTime)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getChangeTime _ = return Nothing
#else
getChangeTime file = Just . epochTimeToUTCTime . statusChangeTime <$> getFileStatus file
#endif

{-|
The 'getModificationTime' operation returns the
UTC time at which the file or directory was last modified.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the modification time; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}
getModificationTime :: FilePath -> IO UTCTime
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getModificationTime file = writeTime <$> fileTime file
#else
getModificationTime file = epochTimeToUTCTime . modificationTime <$> getFileStatus file
#endif

{-
  http://msdn.microsoft.com/en-us/library/ms724290%28VS.85%29.aspx
  The NTFS file system delays updates to the last access time for
  a file by up to 1 hour after the last access.
-}
{-|
The 'getModificationTime' operation returns the
UTC time at which the file or directory was last accessed.
-}
getAccessTime :: FilePath -> IO UTCTime
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getAccessTime file = accessTime <$> fileTime file
#else
getAccessTime file = epochTimeToUTCTime . accessTime <$> getFileStatus file
#endif

----------------------------------------------------------------

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- Open a file or directory for getting the file metadata.
withFileForInfo :: FilePath -> (HANDLE -> IO a) -> IO a
withFileForInfo file = bracket setup teardown
  where
    setup = createFile file 0 fILE_SHARE_READ Nothing
                       oPEN_EXISTING fILE_FLAG_BACKUP_SEMANTICS Nothing
    teardown = closeHandle
#endif

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
creationTime :: (UTCTime,UTCTime,UTCTime) -> UTCTime
creationTime (ctime,_,_) = ctime

accessTime :: (UTCTime,UTCTime,UTCTime) -> UTCTime
accessTime (_,atime,_) = atime

writeTime :: (UTCTime,UTCTime,UTCTime) -> UTCTime
writeTime (_,_,wtime) = wtime

fileTime :: FilePath -> IO (UTCTime,UTCTime,UTCTime)
fileTime file = withFileForInfo file $ \fh -> do
  (ctime,atime,mtime) <- getFileTime fh
  return (filetimeToUTCTime ctime
         ,filetimeToUTCTime atime
         ,filetimeToUTCTime mtime)

{-
  http://support.microsoft.com/kb/167296/en-us
  100 nano seconds since 1 Jan 1601
  MS: _FILETIME = {DWORD,DWORD} = {Word32,Word32}
  Haskell: FILETIME == DDWORD == Word64
-}
filetimeToUTCTime :: FILETIME -> UTCTime
filetimeToUTCTime (FILETIME x) = posixSecondsToUTCTime . realToFrac $ tm
  where
    tm :: Integer
    tm = (fromIntegral x - 116444736000000000) `div` 10000000
#else
epochTimeToUTCTime :: EpochTime -> UTCTime
epochTimeToUTCTime = posixSecondsToUTCTime . realToFrac
#endif

-- | Getting the size of the file.
getFileSize :: FilePath -> IO Word64
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getFileSize file = withFileForInfo file $ \fh ->
  fromIntegral . bhfiSize <$> getFileInformationByHandle fh
#else
getFileSize file = fromIntegral . fileSize <$> getFileStatus file
#endif
