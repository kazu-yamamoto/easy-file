{-|
This module is for file handling in cross-platform environment
including Unix\/Mac\/Windows.

The standard module "System.Directory" and "System.FilePath" have
following shortcomings:

* getModificationTime exists in "System.Directory". But getAccessTime,
  getChangeTime, getCreationTime do not exist.

* getModificationTime returns obsoleted type, 'ClockTime'. It should
  return modern type, 'UTCTime', I believe.

* Path separator is not unified. Even though Windows accepts \'\/\' as a
  file separator, getCurrentDirectory in "System.Directory" returns \'\\\'
  as a file separator. So, we need to specify regular expression like
  this: \"[\/\\\\]foo[\/\\\\]bar[\/\\\\]baz\".

* getHomeDirectory returns HOMEDRIVE\/HOMEPATH instead of the HOME
  environment variable on Windows.

This module aims to resolve these problems and provides:

* 'getModificationTime', 'getAccessTime', 'getChangeTime', and
  'getCreationTime'. They return 'UTCTime'.

* 'isSymlink', 'getLinkCount', and 'hasSubDirectories'.

* \'\/\' as the single 'pathSeparator'. For instance,
  'getCurrentDirectory' returns a path whose separator is \'\/\'
  even on Windows.

* 'getHomeDirectory2' which refers the HOME environment value.

* Necessary functions in "System.Directory" and "System.FilePath".

-}

module System.EasyFile (
  -- * Actions on directories
    createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , removeDirectoryRecursive
  , renameDirectory
  , getDirectoryContents
  , getCurrentDirectory
  , setCurrentDirectory
  -- * Pre-defined directories
  , getHomeDirectory
  , getHomeDirectory2 -- missing
  , getAppUserDataDirectory
  , getUserDocumentsDirectory
  , getTemporaryDirectory
  -- * Actions on files
  , removeFile
  , renameFile
  , copyFile
--   , canonicalizePath -- xxx
--   , makeRelativeToCurrentDirectory -- xxx
--   , findExecutable -- xxx
  -- * Existence tests
  , doesFileExist
  , doesDirectoryExist
  -- * Permissions
  , Permissions(..)
  , getPermissions
  , setPermissions
  , copyPermissions
  -- * Timestamps
  , getCreationTime
  , getChangeTime
  , getModificationTime
  , getAccessTime
  -- * File\/directory information
  , isSymlink
  , getLinkCount
  , hasSubDirectories
  -- * Separator predicates
  , FilePath
  , pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator
  -- * Path methods (environment $PATH)
  , splitSearchPath
  , getSearchPath
  -- * Extension methods
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
  -- * Drive methods
  , splitDrive
  , joinDrive
  , takeDrive
  , hasDrive
  , dropDrive
  , isDrive
  -- * Operations on a FilePath, as a list of directories
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
  -- * Low level FilePath operators
  , hasTrailingPathSeparator
  , addTrailingPathSeparator
  , dropTrailingPathSeparator
  -- * File name manipulators
--  , normalise -- xxx
--  , equalFilePath -- xxx
--  , makeRelative -- xxx
  , isRelative
  , isAbsolute
  , isValid
  , makeValid
  ) where

----------------------------------------------------------------

import System.EasyFile.Directory
import System.EasyFile.FilePath
import System.EasyFile.Missing
