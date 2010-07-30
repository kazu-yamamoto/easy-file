{-# LANGUAGE CPP #-}

module System.EasyFile.FilePath (
    module System.FilePath.Posix
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  , module System.FilePath.Windows
#endif
  ) where

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

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.FilePath.Windows (
    isRelative
  , isAbsolute
  , isValid
  , makeValid
  )
#else
import System.FilePath.Posix (
    isRelative
  , isAbsolute
  , isValid
  , makeValid
  )
#endif
