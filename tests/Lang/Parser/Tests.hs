module Lang.Parser.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assert)

import Data.Either (isLeft)
import Text.Parsec (parse)

import Lang.Core
import Lang.Parser

testVar :: Assertion
testVar = parse var "" s @?= Right (EVar s)
  where
    s = "abc123"

testInvalidVars :: Assertion
testInvalidVars = assert . all isLeft $
    [ parse var "" "1abc"
    , parse var "" " abc"
    , parse var "" ".abc"
    , parse var "" ""
    ]

testPositiveInteger :: Assertion
testPositiveInteger = parse literal "" "123" @?= Right (ELit (LInt 123))

testApVar :: Assertion
testApVar = parse application "" "f x y" @?= expected
  where
    expected = Right $ EAp (EAp (EVar "f") (EVar "x")) (EVar "y")

testApIntegers :: Assertion
testApIntegers = parse application "" "f 1" @?= expected
  where
    expected = Right $ EAp (EVar "f") (ELit (LInt 1))

testNestedAp :: Assertion
testNestedAp = parse application "" "f (g 1)" @?= expected
  where
    expected = Right $ EAp (EVar "f") (EAp (EVar "g") (ELit (LInt 1)))

-- | Tests that expression parsing consumes as much of a string as possible.
testMaximalExpr :: Assertion
testMaximalExpr = parse expr "" "f 1 2" @?= expected
  where
    expected = Right $ EAp (EAp (EVar "f") (ELit (LInt 1))) (ELit (LInt 2))

testNoArgsBinding :: Assertion
testNoArgsBinding = parse binding "" "let f = 2" @?= expected
  where
    expected = Right $ Binding
        { identifier = "f"
        , arguments = []
        , body = ELit (LInt 2)
        }

testArgsBinding :: Assertion
testArgsBinding = parse binding "" "let f x y = x y" @?= expected
  where
    expected = Right $ Binding
        { identifier = "f"
        , arguments = ["x", "y"]
        , body = EAp (EVar "x") (EVar "y")
        }

testProgram :: Assertion
testProgram = fmap (map Right) (parse program "" content) @?= expected
  where
    content = f ++ "\n" ++ g
    f = "let f x = x"
    g = "let g x = x"
    expected = Right [parsedF, parsedG]
    parsedF = parse binding "" f
    parsedG = parse binding "" g

tests :: TestTree
tests = testGroup "Lang.Parser"
    [ testGroup "Expression"
        [ testGroup "Literal"
            [ testCase "Positive integer" testPositiveInteger
            ]
        , testGroup "Variable"
            [ testCase "Valid" testVar
            , testCase "Invalid" testInvalidVars
            ]
        , testGroup "Application"
            [ testCase "Var" testApVar
            , testCase "Integers" testApIntegers
            , testCase "Nested" testNestedAp
            ]
        , testCase "Maximal" testMaximalExpr
        ]
    , testGroup "Binding"
        [ testCase "No arguments" testNoArgsBinding
        , testCase "Arguments" testArgsBinding
        ]
    , testCase "Program" testProgram
    ]

