{-# LANGUAGE CPP #-}

module Test where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.EasyFile

----------------------------------------------------------------

tests :: [Test]
tests = [
    testGroup "FilePath" [
         testCase "</>" test_slash
       , testCase "isRelative1" test_relative1
       , testCase "isRelative2" test_relative2
       , testCase "isRelative3" test_relative3
       , testCase "isRelative4" test_relative4
       ]
    ]

----------------------------------------------------------------

test_slash :: Assertion
test_slash = "foo" </> "bar" @?= "foo/bar"

test_relative1 :: Assertion
test_relative1 = isRelative "foo" @?= True

test_relative2 :: Assertion
test_relative2 = isRelative "/foo" @?=
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    True
#else
    False
#endif

test_relative3 :: Assertion
test_relative3 = isRelative "c:foo" @?=
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    True
#else
    True
#endif

test_relative4 :: Assertion
test_relative4 = isRelative "c:/foo" @?=
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    False
#else
    True
#endif

----------------------------------------------------------------

main :: Assertion
main = defaultMain tests
