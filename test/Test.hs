{-# LANGUAGE CPP #-}

module Main where

import System.EasyFile
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testCase "easy-file tests" $ do

    assertEqual "Test that '/' is path separator"
        ("foo" </> "bar")
        "foo/bar"

    isRelative "foo" @?= True

    isRelative "/foo" @?= isWindows

    isRelative "c:foo" @?= True

    isRelative "c:/foo" @?= not isWindows

isWindows :: Bool
isWindows =
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    True
#else
    False
#endif
