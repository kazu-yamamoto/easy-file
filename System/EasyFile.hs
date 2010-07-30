{-# LANGUAGE CPP #-}

{-
- isRelative in System.FilePath
-}

module System.EasyFile (
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    module System.EasyFile.Win32
#else
    module System.EasyFile.Posix
#endif
  , FilePath
  , pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator
  , splitSearchPath
  , getSearchPath
  , splitExtension
  , takeExtension
  , replaceExtension
  , dropExtension
  , addExtension
  , hasExtension
  , (<.>)
  , splitExtensions
  , dropExtensions
  , takeExtensions
  , splitDrive
  , joinDrive
  , takeDrive
  , hasDrive
  , dropDrive
  , isDrive
  , splitFileName
  , takeFileName
  , replaceFileName
  , dropFileName
  , takeBaseName
  , replaceBaseName
  , takeDirectory
  , replaceDirectory
  , combine
  , (</>)
  , splitPath
  , joinPath
  , splitDirectories
  , hasTrailingPathSeparator
  , addTrailingPathSeparator
  , dropTrailingPathSeparator
  , createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , removeDirectoryRecursive
  , renameDirectory
  , getDirectoryContents
  , setCurrentDirectory
  , removeFile
  , renameFile
  , copyFile
  , doesFileExist
  , doesDirectoryExist
  , Permissions(..)
  , getPermissions
  , setPermissions
  , copyPermissions
  ) where

----------------------------------------------------------------

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.EasyFile.Win32
#else
import System.EasyFile.Posix
#endif

----------------------------------------------------------------

import System.FilePath.Posix (
    FilePath
  , pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator
  , splitSearchPath
  , getSearchPath
  , splitExtension
  , takeExtension
  , replaceExtension
  , dropExtension
  , addExtension
  , hasExtension
  , (<.>)
  , splitExtensions
  , dropExtensions
  , takeExtensions
  , splitDrive
  , joinDrive
  , takeDrive
  , hasDrive
  , dropDrive
  , isDrive
  , splitFileName
  , takeFileName
  , replaceFileName
  , dropFileName
  , takeBaseName
  , replaceBaseName
  , takeDirectory
  , replaceDirectory
  , combine
  , (</>)
  , splitPath
  , joinPath
  , splitDirectories
  , hasTrailingPathSeparator
  , addTrailingPathSeparator
  , dropTrailingPathSeparator
  )

----------------------------------------------------------------

import System.Directory (
    createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , removeDirectoryRecursive
  , renameDirectory
  , getDirectoryContents
  , setCurrentDirectory
  , removeFile
  , renameFile
  , copyFile
  , doesFileExist
  , doesDirectoryExist
  , Permissions(..)
  , getPermissions
  , setPermissions
  , copyPermissions
  )
