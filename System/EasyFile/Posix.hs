{-# LANGUAGE CPP #-}

module System.EasyFile.Posix (
    module System.EasyFile.Posix
  , module System.Directory
  ) where

----------------------------------------------------------------

import Control.Applicative
import Data.Time
import Data.Time.Clock.POSIX
import System.Posix.Files
import System.Posix.Types
import System.Directory (
    getCurrentDirectory
  , getHomeDirectory
  , getAppUserDataDirectory
  , getUserDocumentsDirectory
  , getTemporaryDirectory
  , canonicalizePath
  )

----------------------------------------------------------------

isSymlink :: FilePath -> IO Bool
isSymlink file = isSymbolicLink <$> getSymbolicLinkStatus file

getLinkCount :: FilePath -> IO (Maybe Int)
getLinkCount file = Just . fromIntegral . linkCount <$> getFileStatus file

----------------------------------------------------------------

getCreationTime :: FilePath -> IO (Maybe UTCTime)
getCreationTime _ = return Nothing

{-
  Unix's rename() does not change ctime but MacOS's rename() changes, sigh.
-}
getChangeTime :: FilePath -> IO (Maybe UTCTime)
getChangeTime file = Just . epochTimeToUTCTime . statusChangeTime <$> getFileStatus file

getModificationTime :: FilePath -> IO UTCTime
getModificationTime file = epochTimeToUTCTime . modificationTime <$> getFileStatus file

getAccessTime :: FilePath -> IO UTCTime
getAccessTime file = epochTimeToUTCTime . accessTime <$> getFileStatus file

epochTimeToUTCTime :: EpochTime -> UTCTime
epochTimeToUTCTime = posixSecondsToUTCTime . realToFrac

hasSubDirectories :: FilePath -> IO (Maybe Bool)
#ifdef darwin_HOST_OS
hasSubDirectories _ = return Nothing
#else
hasSubDirectories file = do
  Just n <- getLinkCount file
  return $ Just (n > 2)
#endif
