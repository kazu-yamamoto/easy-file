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
         ]
    ]

----------------------------------------------------------------

test_slash :: Assertion
test_slash = "foo" </> "bar" @?= "foo/bar"

----------------------------------------------------------------

main :: Assertion
main = defaultMain tests
