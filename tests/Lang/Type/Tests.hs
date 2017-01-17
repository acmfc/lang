module Lang.Type.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import Lang.Type

testTypeVariables :: Assertion
testTypeVariables =
  typeVariables (arrow (TVar "t1") (TVar "t2")) @?= ["t1", "t2"]

tests :: TestTree
tests = testGroup "Lang.Type" [testCase "testTypeVariables" testTypeVariables]
