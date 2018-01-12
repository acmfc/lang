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
testTypeVariable = parseTyp tv @?= Right expected
  where
    tv = "abc"
    expected = T.genEmptyEnv . T.Qual [] $ T.TVar $ T.Tyvar tv T.KStar

testTypeConstant :: Assertion
testTypeConstant = parseTyp tc @?= Right expected
  where
    tc = "Abc"
    expected = T.genEmptyEnv . T.Qual [] $ T.TCon $ T.Tycon tc T.KStar

testTypeParens :: Assertion
testTypeParens = parseTyp "((a))" @?= Right expected
  where
    expected = T.genEmptyEnv . T.Qual [] $ T.TVar (T.Tyvar "a" T.KStar)

testTypeFunction :: Assertion
testTypeFunction = parseTyp "a -> b" @?= Right expected
  where
    expected = T.genEmptyEnv . T.Qual [] $ T.makeFun tv1 tv2
    tv1 = T.TVar (T.Tyvar "a" T.KStar)
    tv2 = T.TVar (T.Tyvar "b" T.KStar)

testTypeFunctionAssoc :: Assertion
testTypeFunctionAssoc = parseTyp "a -> b -> c" @?= Right expected
  where
    expected =
        T.genEmptyEnv . T.Qual [] $ T.makeFun (T.TVar (T.Tyvar "a" T.KStar)) f
    f = T.makeFun (T.TVar (T.Tyvar "b" T.KStar)) (T.TVar (T.Tyvar "c" T.KStar))

testTypeFunctionParens :: Assertion
testTypeFunctionParens = parseTyp "((a -> b) -> c)" @?= Right expected
  where
    expected =
        T.genEmptyEnv . T.Qual [] $ T.makeFun f (T.TVar (T.Tyvar "c" T.KStar))
    f = T.makeFun (T.TVar (T.Tyvar "a" T.KStar)) (T.TVar (T.Tyvar "b" T.KStar))

testTypeRecord :: Assertion
testTypeRecord = parseTyp toParse @?= Right expected
  where
    toParse = "{ x : Int | r } -> { y : Int, z : Int }"
    expected = T.genEmptyEnv . T.Qual preds $ t
    preds = [ T.RowEq (T.RVar var4) (T.RExt y T.tInt (T.RVar var3))
            , T.RowLacks (T.RVar var3) y
            , T.RowEq (T.RVar var3) (T.RExt z T.tInt (T.RVar var2))
            , T.RowLacks (T.RVar var2) z
            , T.RowEq (T.RVar var1) (T.RExt x T.tInt (T.RVar varBase))
            , T.RowLacks (T.RVar varBase) x
            ]
    t = T.makeFun (T.TAp T.tRecordCon var1) (T.TAp T.tRecordCon var4)
    varBase = T.TVar $ T.Tyvar "r" T.KRow
    var1 = T.TVar $ T.Tyvar "$r1" T.KRow
    var2 = T.TVar $ T.Tyvar "$r2" T.KRow
    var3 = T.TVar $ T.Tyvar "$r3" T.KRow
    var4 = T.TVar $ T.Tyvar "$r4" T.KRow
    x = T.TVar $ T.Tyvar "x" T.KLab
    y = T.TVar $ T.Tyvar "y" T.KLab
    z = T.TVar $ T.Tyvar "z" T.KLab

testTypeFinalRecord :: Assertion
testTypeFinalRecord = parseTyp toParse @?= Right expected
  where
    toParse = "{| x : Int |}"
    expected = T.genEmptyEnv . T.Qual preds $ t
    preds = [ T.RowEq (T.RVar var1) (T.RExt x T.tInt (T.RVar var0))
            , T.RowLacks (T.RVar var0) x
            , T.RowEq (T.RVar var0) T.REmpty
            ]
    t = T.TAp T.tRecordCon var1
    var0 = T.TVar $ T.Tyvar "$r0" T.KRow
    var1 = T.TVar $ T.Tyvar "$r1" T.KRow
    x = T.TVar $ T.Tyvar "x" T.KLab

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
        [ testCase "Variable" testTypeVariable
        , testCase "Constant" testTypeConstant
        , testCase "Parens" testTypeParens
        , testCase "Function" testTypeFunction
        , testCase "Function associativity" testTypeFunctionAssoc
        , testCase "Function parens" testTypeFunctionParens
        , testCase "Record" testTypeRecord
        , testCase "Final record" testTypeFinalRecord
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

