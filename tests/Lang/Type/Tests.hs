module Lang.Type.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assert)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Lang.Type
import Lang.Core

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

-- | Define an environment for use by tests containing basic operations.
defaultEnv :: TypeEnv
defaultEnv = TypeEnv (Map.fromList [("+", toScheme t)])
  where
    t = makeFun tInt (makeFun tInt tInt)

testTiExprId :: Assertion
testTiExprId = runTI (tiExpr defaultEnv e) @?= ([], expectedType)
  where
    e = ELet [idBinding] (EVar "id")
    idBinding = Binding {identifier="id", arguments=["a"], body=EVar "a"}
    expectedType = makeFun (TVar (tvs !! 2)) (TVar (tvs !! 2))

-- | Check that binding groups containing more than a minimal set of mutually
-- recursive definitions will unnecessarily restrict polymorphism.
testLargeBindingGroup :: Assertion
testLargeBindingGroup = tiProgram defaultEnv program @?= expectedEnv
  where
    expectedEnv = TypeEnv (Map.fromList
                  [ ("+", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("f", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("id", toScheme $ makeFun tInt tInt)
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
                  [ ("+", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("f", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("id", Scheme [tvs !! 1] (Qual [] tId))
                  ])
    tId = makeFun (TVar (tvs !! 1)) (TVar (tvs !! 1))
    program = [ [Binding {identifier="id", arguments=["a"], body=EVar "a"}]
              , [Binding {identifier="f", arguments=fArgs, body=fBody}]
              ]
    fArgs = ["a", "b"]
    fBody = EAp (EAp (EVar "+") (EVar "a")) (EAp (EVar "id") (EVar "b"))

testTiProgram :: Assertion
testTiProgram = tiProgram defaultEnv program @?= expectedEnv
  where
    expectedEnv = TypeEnv (Map.fromList
                  [ ("+", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("f", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("id", Scheme [tvs !! 1] (Qual [] tId))
                  ])
    tId = makeFun (TVar (tvs !! 1)) (TVar (tvs !! 1))
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
