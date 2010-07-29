{-# LANGUAGE CPP #-}

{-

- Provide getStatusChangeTime in addition to getModificationTime.
- Use UTCTime instead of ClockTime
- Single file separator '/'. (</>) should use '/' only.
  getCurrentDirectory and getHomeDirectory should convert '\' to '/'.

-}

module System.EasyFile (
    isSymlink
  , getLinkCount
  , getCreationTime
  , getChangeTime
  , getModificationTime
  , getAccessTime
  , hasSubDirectories
  ) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.EasyFile.Win32
#else
import System.EasyFile.Posix
#endif
