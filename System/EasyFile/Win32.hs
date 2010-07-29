module System.EasyFile.Win32 where

import Control.Applicative
import Data.Time
import Data.Time.Clock.POSIX
import System.Win32.File
import System.Win32.Time

isSymlink :: FilePath -> IO Bool
isSymlink _ = return False

getLinkCount :: FilePath -> IO (Maybe Int)
getLinkCount _ = return Nothing

getCreationTime :: FilePath -> IO (Maybe UTCTime)
getCreationTime file = Just . creationTime <$> fileTime file

getChangeTime :: FilePath -> IO (Maybe UTCTime)
getChangeTime _ = return Nothing

getModificationTime :: FilePath -> IO UTCTime
getModificationTime file = writeTime <$> fileTime file

{-
  http://msdn.microsoft.com/en-us/library/ms724290%28VS.85%29.aspx
  The NTFS file system delays updates to the last access time for
  a file by up to 1 hour after the last access.
-}
getAccessTime :: FilePath -> IO UTCTime
getAccessTime file = accessTime <$> fileTime file

creationTime :: (UTCTime,UTCTime,UTCTime) -> UTCTime
creationTime (ctime,_,_) = ctime

accessTime :: (UTCTime,UTCTime,UTCTime) -> UTCTime
accessTime (_,atime,_) = atime

writeTime :: (UTCTime,UTCTime,UTCTime) -> UTCTime
writeTime (_,_,wtime) = wtime

fileTime :: FilePath -> IO (UTCTime,UTCTime,UTCTime)
fileTime file = do
    fh <- createFile file gENERIC_READ fILE_SHARE_READ Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
    (ctime,atime,mtime) <- getFileTime fh
    closeHandle fh
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
filetimeToUTCTime (FILETIME x) = posixSecondsToUTCTime . realToFrac $ (fromIntegral x - 116444736000000000) `div` 10000000

hasSubDirectories :: FilePath -> IO (Maybe Bool)
hasSubDirectories _ = return Nothing