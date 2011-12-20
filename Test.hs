{-# LANGUAGE CPP, TemplateHaskell #-}

module Test where

import System.EasyFile
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

case_slash :: Assertion
case_slash = "foo" </> "bar" @?= "foo/bar"

case_relative1 :: Assertion
case_relative1 = isRelative "foo" @?= True

case_relative2 :: Assertion
case_relative2 = isRelative "/foo" @?=
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    True
#else
    False
#endif

case_relative3 :: Assertion
case_relative3 = isRelative "c:foo" @?=
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    True
#else
    True
#endif

case_relative4 :: Assertion
case_relative4 = isRelative "c:/foo" @?=
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    False
#else
    True
#endif

----------------------------------------------------------------
