module Lang.DependencyAnalysis.Tests ( tests ) where

import Control.Comonad.Cofree
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import qualified Data.Set as Set

import Lang.Core
import Lang.DependencyAnalysis

testExprDependencies :: Assertion
testExprDependencies = exprDependencies e Set.empty @?= expectedDeps
  where
    e = () :< ELet [fBinding gBinding] e'
    fBinding = Binding { identifier="f", arguments=[""], body=fBody, annot=() }
    gBinding = Binding { identifier="g", arguments=[""], body=gBody, annot=() }
    fBody = () :< EAp (() :< EVar "g") (() :< EVar "a")
    gBody = () :< EAp (() :< EVar "f") (() :< EVar "b")
    e' = () :< EAp (() :< EAp (() :< EVar "+") (() :< EVar "f")) (() :< ELit (LInt 0))
    expectedDeps = Set.fromList ["+", "a", "b"]

testIgnore :: Assertion
testIgnore = exprDependencies e ignore @?= expectedDeps
  where
    e = () :< EAp (() :< EAp (() :< EVar "+") (() :< EVar "a")) (() :< EVar "b")
    ignore = Set.fromList ["+"]
    expectedDeps = Set.fromList ["a", "b"]

testBindingDependencies :: Assertion
testBindingDependencies = bindingDependencies b Set.empty @?= expectedDeps
  where
    b = Binding { identifier="f", arguments=["a"], body=bBody, annot=() }
    bBody =
        () :< EAp (() :< EAp (() :< EVar "f") (() :< EVar "a")) (() :< EVar "b")
    expectedDeps = Set.fromList ["b"]

testStructureBindings :: Assertion
testStructureBindings = structureBindings bs @?= expectedBindingGroups
  where
    -- f and g are mutually recursive. h depends on g. x has no dependencies.
    bs = [ Binding { identifier="h", arguments=[], body=() :< EAp (() :< EVar "g") (() :< ELit (LInt 0)), annot=() }
         , Binding { identifier="g", arguments=["a"], body=() :< EAp (() :< EVar "f") (() :< EVar "a"), annot=() }
         , Binding { identifier="f", arguments=["a"], body=() :< EAp (() :< EVar "g") (() :< EVar "a"), annot=() }
         , Binding { identifier="x", arguments=[], body=() :< ELit (LInt 1), annot=() }
         ]
    expectedBindingGroups =
        [ [ Binding { identifier="x", arguments=[], body=() :< ELit (LInt 1), annot=() } ]
        , [ Binding { identifier="f", arguments=["a"], body=() :< EAp (() :< EVar "g") (() :< EVar "a"), annot=() }
          , Binding { identifier="g", arguments=["a"], body=() :< EAp (() :< EVar "f") (() :< EVar "a"), annot=() }
          ]
        , [ Binding { identifier="h", arguments=[], body=() :< EAp (() :< EVar "g") (() :< ELit (LInt 0)), annot=() } ]
        ]

tests :: TestTree
tests = testGroup "Lang.DependencyAnalysis"
    [ testCase "testExprDependencies" testExprDependencies
    , testCase "testIgnore" testIgnore
    , testCase "testBindingDependencies" testBindingDependencies
    , testCase "testStructureBindings" testStructureBindings
    ]

