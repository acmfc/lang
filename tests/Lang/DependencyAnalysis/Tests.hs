module Lang.DependencyAnalysis.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import qualified Data.Set as Set

import Lang.Core
import Lang.DependencyAnalysis
import Lang.Expr

testExprDependencies :: Assertion
testExprDependencies = exprDependencies e Set.empty @?= expectedDeps
  where
    e = elet [fBinding, gBinding] e'
    fBinding = Binding { identifier = "f"
                       , arguments = [""]
                       , body = fBody
                       , annot = ()
                       }
    gBinding = Binding { identifier = "g"
                       , arguments = [""]
                       , body = gBody
                       , annot = ()
                       }
    fBody = eap (evar "g") (evar "a")
    gBody = eap (evar "f") (evar "b")
    e' = eap (eap (evar "+") (evar "f")) (elit (LInt 0))
    expectedDeps = Set.fromList ["+", "a", "b"]

testIgnore :: Assertion
testIgnore = exprDependencies e ignore @?= expectedDeps
  where
    e = eap (eap (evar "+") (evar "a")) (evar "b")
    ignore = Set.fromList ["+"]
    expectedDeps = Set.fromList ["a", "b"]

testBindingDependencies :: Assertion
testBindingDependencies = bindingDependencies b Set.empty @?= expectedDeps
  where
    b = Binding { identifier = "f"
                , arguments = ["a"]
                , body = bBody
                , annot = ()
                }
    bBody =
        eap (eap (evar "f") (evar "a")) (evar "b")
    expectedDeps = Set.fromList ["b"]

testStructureBindings :: Assertion
testStructureBindings = structureBindings bs @?= expectedBindingGroups
  where
    -- f and g are mutually recursive. h depends on g. x has no dependencies.
    bs = [ Binding { identifier = "h"
                   , arguments = []
                   , body = eap (evar "g") (elit (LInt 0))
                   , annot = ()
                   }
         , Binding { identifier = "g"
                   , arguments = ["a"]
                   , body = eap (evar "f") (evar "a")
                   , annot = ()
                   }
         , Binding { identifier = "f"
                   , arguments = ["a"]
                   , body = eap (evar "g") (evar "a")
                   , annot = ()
                   }
         , Binding { identifier = "x"
                   , arguments = []
                   , body = elit (LInt 1)
                   , annot = ()
                   }
         ]
    expectedBindingGroups =
        [ [Binding { identifier = "x"
                   , arguments = []
                   , body = elit (LInt 1)
                   , annot = ()
                   }]
        , [ Binding { identifier = "f"
                    , arguments = ["a"]
                    , body = eap (evar "g") (evar "a")
                    , annot = ()
                    }
          , Binding { identifier = "g"
                    , arguments = ["a"]
                    , body = eap (evar "f") (evar "a")
                    , annot = ()
                    }
          ]
        , [Binding { identifier = "h"
                   , arguments = []
                   , body = eap (evar "g") (elit (LInt 0))
                   , annot = ()
                   }]
        ]

tests :: TestTree
tests = testGroup "Lang.DependencyAnalysis"
    [ testCase "testExprDependencies" testExprDependencies
    , testCase "testIgnore" testIgnore
    , testCase "testBindingDependencies" testBindingDependencies
    , testCase "testStructureBindings" testStructureBindings
    ]

