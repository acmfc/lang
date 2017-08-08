module Lang.Type.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assert)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Lang.Type

tvs :: [Tyvar]
tvs = map (\n -> Tyvar ("t" ++ show n) KStar) ([0..] :: [Integer])

testTypeVariables :: Assertion
testTypeVariables = typeVariables t @?= Set.fromList [tvs !! 1, tvs !! 2]
      where
        t = makeFun (TVar (tvs !! 1)) (TVar (tvs !! 2))

testCompose :: Assertion
testCompose = assert $ all (== composed t) [defined t, expected]
  where
    composed = apply (compose sub2 sub1)
    defined = apply sub2 . apply sub1
    sub1 = Map.singleton (tvs !! 1) (TVar (tvs !! 2))
    sub2 = Map.fromAscList [ (tvs !! 2, TVar (tvs !! 5))
                           , (tvs !! 6, TVar (tvs !! 7))
                           ]
    t = makeFun (TVar (tvs !! 1)) (TVar (tvs !! 6))
    expected = makeFun (TVar (tvs !! 5)) (TVar (tvs !! 7))

tests :: TestTree
tests = testGroup "Lang.Type"
    [ testCase "Type Variables" testTypeVariables
    , testCase "Compose" testCompose
    ]

