module Lang.Type.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assert)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Lang.Type
import Lang.Core

testTypeVariables :: Assertion
testTypeVariables =
    typeVariables (TVar "t1" `TFun` TVar "t2") @?= Set.fromList ["t1", "t2"]

testCompose :: Assertion
testCompose = assert $ same [composed t, defined t, expected]
  where
    same (x:xs) = all (== x) xs
    same [] = True
    composed = apply (compose sub2 sub1)
    defined = (apply sub2 . apply sub1)
    sub1 = Map.singleton "t1" (TVar "t2")
    sub2 = Map.fromAscList [("t2", TVar "t5"), ("t6", TVar "t7")]
    t = TVar "t1" `TFun` TVar "t6"
    expected = TVar "t5" `TFun` TVar "t7"

-- | Define an environment for use by tests containing basic operations.
defaultEnv :: TypeEnv
defaultEnv = TypeEnv
    (Map.fromList [("+", toScheme (TFun TInt (TFun TInt TInt)))])

testTiExprId :: Assertion
testTiExprId = runTI (tiExpr defaultEnv e) @?= expectedType
  where
    e = ELet [idBinding] (EVar "id")
    idBinding = Binding {identifier="id", arguments=["a"], body=EVar "a"}
    expectedType = TFun (TVar "t2") (TVar "t2")

-- | Check that binding groups containing more than a minimal set of mutually
-- recursive definitions will unnecessarily restrict polymorphism.
testLargeBindingGroup :: Assertion
testLargeBindingGroup = tiProgram defaultEnv program @?= expectedEnv
  where
    expectedEnv = TypeEnv (Map.fromList
                  [ ("+", toScheme $ foldr TFun TInt [TInt, TInt])
                  , ("f", toScheme $ foldr TFun TInt [TInt, TInt])
                  , ("id", toScheme $ TFun TInt TInt)
                  ])
    program = [[ Binding {identifier="id", arguments=["a"], body=EVar "a"}
               , Binding {identifier="f", arguments=fArgs, body=fBody}
               ]]
    fArgs = ["a", "b"]
    fBody = EAp (EAp (EVar "+") (EVar "a")) (EAp (EVar "id") (EVar "b"))

-- | Check that properly minimized binding groups result in maximally
-- polymorphic types.
testSmallBindingGroup :: Assertion
testSmallBindingGroup = tiProgram defaultEnv program @?= expectedEnv
  where
    expectedEnv = TypeEnv (Map.fromList
                  [ ("+", toScheme $ foldr TFun TInt [TInt, TInt])
                  , ("f", toScheme $ foldr TFun TInt [TInt, TInt])
                  , ("id", Scheme ["t1"] (TFun (TVar "t1") (TVar "t1")))
                  ])
    program = [ [Binding {identifier="id", arguments=["a"], body=EVar "a"}]
              , [Binding {identifier="f", arguments=fArgs, body=fBody}]
              ]
    fArgs = ["a", "b"]
    fBody = EAp (EAp (EVar "+") (EVar "a")) (EAp (EVar "id") (EVar "b"))

testTiProgram :: Assertion
testTiProgram = tiProgram defaultEnv program @?= expectedEnv
  where
    expectedEnv = TypeEnv (Map.fromList
                  [ ("+", toScheme $ foldr TFun TInt [TInt, TInt])
                  , ("f", toScheme $ foldr TFun TInt [TInt, TInt])
                  , ("id", Scheme ["t1"] (TFun (TVar "t1") (TVar "t1")))
                  ])
    program = [ [Binding {identifier="id", arguments=["a"], body=EVar "a"}]
              , [Binding {identifier="f", arguments=fArgs, body=fBody}]
              ]
    fArgs = ["a", "b"]
    fBody = EAp (EAp (EVar "+") (EVar "a")) (EVar "b")

tests :: TestTree
tests = testGroup "Lang.Type"
    [ testGroup "Type Representation"
        [ testCase "testTypeVariables" testTypeVariables
        , testCase "testCompose" testCompose
        ]
    , testGroup "Type Inference"
        [ testCase "testTiExprId" testTiExprId
        , testCase "testTiProgram" testTiProgram
        , testCase "testLargeBindingGroup" testLargeBindingGroup
        , testCase "testSmallBindingGroup" testSmallBindingGroup
        ]
    ]
