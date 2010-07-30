{-# LANGUAGE CPP #-}

module System.EasyFile.Directory (
    module System.Directory
  ) where

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
