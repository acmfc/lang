module Lang.Parser.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assert)

import Data.Either (isLeft)
import Text.Parsec (parse)

import Lang.Core
import Lang.Expr
import Lang.Parser
import qualified Lang.Type as T

testTypeVariable :: Assertion
testTypeVariable = parse typ "" tv @?= Right expected
  where
    tv = "abc"
    expected = T.genEmptyEnv . T.Qual [] $ T.TVar $ T.Tyvar tv T.KStar

testTypeConstant :: Assertion
testTypeConstant = parse typ "" tc @?= Right expected
  where
    tc = "Abc"
    expected = T.genEmptyEnv . T.Qual [] $ T.TCon $ T.Tycon tc T.KStar

testTypeParens :: Assertion
testTypeParens = parse typ "" "((a))" @?= Right expected
  where
    expected = T.genEmptyEnv . T.Qual [] $ T.TVar (T.Tyvar "a" T.KStar)

testTypeFunction :: Assertion
testTypeFunction = parse typ "" "a -> b" @?= Right expected
  where
    expected = T.genEmptyEnv . T.Qual [] $ T.makeFun tv1 tv2
    tv1 = T.TVar (T.Tyvar "a" T.KStar)
    tv2 = T.TVar (T.Tyvar "b" T.KStar)

testTypeFunctionAssoc :: Assertion
testTypeFunctionAssoc = parse typ "" "a -> b -> c" @?= Right expected
  where
    expected =
        T.genEmptyEnv . T.Qual [] $ T.makeFun (T.TVar (T.Tyvar "a" T.KStar)) f
    f = T.makeFun (T.TVar (T.Tyvar "b" T.KStar)) (T.TVar (T.Tyvar "c" T.KStar))

testTypeFunctionParens :: Assertion
testTypeFunctionParens = parse typ "" "((a -> b) -> c)" @?= Right expected
  where
    expected =
        T.genEmptyEnv . T.Qual [] $ T.makeFun f (T.TVar (T.Tyvar "c" T.KStar))
    f = T.makeFun (T.TVar (T.Tyvar "a" T.KStar)) (T.TVar (T.Tyvar "b" T.KStar))

testVar :: Assertion
testVar = parse var "" s @?= Right (evar s)
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
testPositiveInteger =
    parse literal "" "123" @?= Right (elit (LInt 123))

testApVar :: Assertion
testApVar = parse application "" "f x y" @?= expected
  where
    expected = Right $ eap (eap (evar "f") (evar "x")) (evar "y")

testApIntegers :: Assertion
testApIntegers = parse application "" "f 1" @?= expected
  where
    expected = Right $ eap (evar "f") (elit (LInt 1))

testNestedAp :: Assertion
testNestedAp = parse application "" "f (g 1)" @?= expected
  where
    expected = Right $ eap (evar "f") (eap (evar "g") (elit (LInt 1)))

-- | Tests that expression parsing consumes as much of a string as possible.
testMaximalExpr :: Assertion
testMaximalExpr = parse expr "" "f 1 2" @?= expected
  where
    expected = Right $ eap (eap (evar "f") (elit (LInt 1))) (elit (LInt 2))

testNoArgsBinding :: Assertion
testNoArgsBinding = parse binding "" "let f = 2" @?= expected
  where
    expected = Right Binding
        { identifier = "f"
        , arguments = []
        , body = elit (LInt 2)
        , annot = Nothing
        }

testArgsBinding :: Assertion
testArgsBinding = parse binding "" "let f x y = x y" @?= expected
  where
    expected = Right Binding
        { identifier = "f"
        , arguments = ["x", "y"]
        , body = eap (evar "x") (evar "y")
        , annot = Nothing
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
    [ testGroup "Type"
        [ testCase "Type variable" testTypeVariable
        , testCase "Type constant" testTypeConstant
        , testCase "Type parens" testTypeParens
        , testCase "Type function" testTypeFunction
        , testCase "Type function associativity" testTypeFunctionAssoc
        , testCase "Type function parens" testTypeFunctionParens
        ]
    , testGroup "Expression"
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

