{-# LANGUAGE ScopedTypeVariables #-}
module Lang.Type.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import qualified Data.Map as Map

import Lang.Type

testTypeVariables :: Assertion
testTypeVariables =
  typeVariables (TVar "t1" `arrow` TVar "t2") @?= ["t1", "t2"]

testCompose :: Assertion
testCompose = apply (compose sub2 sub1) t @?= (apply sub2 . apply sub1) t
 where
  sub1 = Map.singleton "t3" (TVar "t4")
  sub2 = Map.fromAscList [("t2", TVar "t5"), ("t6", TVar "t7")]
  t = TVar "t1" `arrow` TVar "t6"

tests :: TestTree
tests = testGroup "Lang.Type"
  [ testCase "testTypeVariables" testTypeVariables
  , testCase "testCompose" testCompose
  ]
