{-# LANGUAGE CPP #-}

module Main where

import Control.Exception (bracket)
import System.EasyFile
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain $
        testGroup
            "easy-file"
            [ testCase "path" testPath
            , testCase "setFileSize" testSetFileSize
            , testCase "hasSubDirectories" testHasSubDirectories
            , testCase "metadata of a file open for writing" testOpenForWriting
            ]

testPath :: Assertion
testPath = do
    assertEqual
        "Test that '/' is path separator"
        ("foo" </> "bar")
        "foo/bar"

    isRelative "foo" @?= True

    isRelative "/foo" @?= isWindows

    isRelative "c:foo" @?= True

    isRelative "c:/foo" @?= not isWindows

-- The file must not be left open: getting its size and removing it work.
testSetFileSize :: Assertion
testSetFileSize = withTempDir "setFileSize" $ \dir -> do
    let file = dir </> "file"
    writeFile file "0123456789"
    setFileSize file 4
    getFileSize file >>= (@?= 4)
    setFileSize file 16
    getFileSize file >>= (@?= 16)
    removeFile file

testHasSubDirectories :: Assertion
testHasSubDirectories = withTempDir "hasSubDirectories" $ \dir -> do
    createDirectory $ dir </> "sub"
    r <- hasSubDirectories dir
    if isWindows || isMac
        then r @?= Nothing
        else r @?= Just True

testOpenForWriting :: Assertion
testOpenForWriting = withTempDir "openForWriting" $ \dir -> do
    let file = dir </> "file"
    withFile file WriteMode $ \h -> do
        hPutStr h "0123456789" >> hFlush h
        _ <- getModificationTime file
        _ <- getAccessTime file
        getFileSize file >>= (@?= 10)

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name = bracket setup removeDirectoryRecursive
  where
    setup = do
        tmp <- getTemporaryDirectory
        let dir = tmp </> ("easy-file-test-" ++ name)
        exist <- doesDirectoryExist dir
        if exist then removeDirectoryRecursive dir else return ()
        createDirectory dir
        return dir

isWindows :: Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isWindows = True
#else
isWindows = False
#endif

isMac :: Bool
#ifdef darwin_HOST_OS
isMac = True
#else
isMac = False
#endif
