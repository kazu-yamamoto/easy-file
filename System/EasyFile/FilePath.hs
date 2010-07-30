{-# LANGUAGE CPP #-}

module System.EasyFile.FilePath (
    module System.EasyFile.FilePath
  , module System.FilePath.Posix
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  , module System.FilePath.Windows
#endif
  ) where

import System.FilePath.Posix (
    FilePath
  , isPathSeparator
--  , searchPathSeparator  -- xxx
--  , isSearchPathSeparator -- xxx
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

import qualified System.FilePath.Posix as F (
    pathSeparator
  )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.FilePath.Windows (
#else
import System.FilePath.Posix (
#endif
    isRelative
  , isAbsolute
  , isValid
  , makeValid
  )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.FilePath.Windows as P (
#else
import qualified System.FilePath.Posix as P (
#endif
    pathSeparators
  )

-- | The character that separates directories.
--
-- > pathSeparator == '/'
pathSeparator :: Char
pathSeparator = F.pathSeparator

-- | The list of all possible separators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
pathSeparators :: [Char]
pathSeparators = P.pathSeparators
