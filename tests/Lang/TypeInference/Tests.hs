module Lang.TypeInference.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import Control.Comonad.Cofree
import Text.Parsec (parse)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Lang.Core
import Lang.Expr
import Lang.Type
import Lang.TypeInference
import qualified Lang.Parser as Parser

tvs :: [Tyvar]
tvs = map (\n -> Tyvar ("$t" ++ show n) KStar) ([0..] :: [Integer])

tIntBinop :: Type
tIntBinop = foldr makeFun tInt [tInt, tInt]

tIntUnop :: Type
tIntUnop = makeFun tInt tInt

parseType :: String -> Maybe Scheme
parseType s = case parse Parser.typ "" s of
    Left _ -> Nothing
    Right scheme -> Just scheme

-- | Define an environment for use by tests containing basic operations.
defaultEnv :: TypeEnv
defaultEnv = TypeEnv (Map.fromList [("+", toScheme tIntBinop)])

twoOfThree :: (a, b, c) -> b
twoOfThree (_, x, _) = x

testTiExprId :: Assertion
testTiExprId = (ps, inferredType) @?= ([], expectedType)
  where
    Right (ps, inferredType :< _) = runTI (tiExpr defaultEnv e)
    e = elet [idBinding] (evar "id")
    idBinding = Binding { identifier = "id"
                        , arguments = ["a"]
                        , body = evar "a"
                        , annot = Nothing
                        }
    expectedType = makeFun (TVar (tvs !! 2)) (TVar (tvs !! 2))

-- | Check that binding groups containing more than a minimal set of mutually
-- recursive definitions will unnecessarily restrict polymorphism.
testLargeBindingGroup :: Assertion
testLargeBindingGroup =
    fmap twoOfThree (tiProgram defaultEnv program) @?= Right expectedEnv
      where
        expectedEnv = TypeEnv (Map.fromList
                      [ ("+", toScheme tIntBinop)
                      , ("f", toScheme tIntBinop)
                      , ("id", toScheme tIntUnop)
                      ])
        program = [[ Binding { identifier = "id"
                             , arguments = ["a"]
                             , body = evar "a"
                             , annot = Nothing
                             }
                   , Binding { identifier = "f"
                             , arguments = fArgs
                             , body = fBody
                             , annot = Nothing
                             }
                   ]]
        fArgs = ["a", "b"]
        fBody = eap (eap (evar "+") (evar "a")) (eap (evar "id") (evar "b"))

-- | Check that properly minimized binding groups result in maximally
-- polymorphic types.
testMinimalBindingGroup :: Assertion
testMinimalBindingGroup =
    fmap twoOfThree (tiProgram defaultEnv program) @?= Right expectedEnv
      where
        expectedEnv = TypeEnv $ Map.fromList
                [ ("+", toScheme tIntBinop)
                , ("f", toScheme tIntBinop)
                , ("id", Scheme [tvs !! 1] (Qual [] tId))
                ]
        tId = makeFun (TVar (tvs !! 1)) (TVar (tvs !! 1))
        program = [ [Binding { identifier = "id"
                             , arguments = ["a"]
                             , body = evar "a"
                             , annot = Nothing
                             }]
                  , [Binding { identifier = "f"
                             , arguments = fArgs
                             , body = fBody
                             , annot = Nothing
                             }]
                  ]
        fArgs = ["a", "b"]
        fBody = eap (eap (evar "+") (evar "a")) (eap (evar "id") (evar "b"))

testExplicitBinding :: Assertion
testExplicitBinding =
    fmap twoOfThree (tiProgram defaultEnv program) @?= Right expectedEnv
      where
        program = [[Binding { identifier = "f"
                            , arguments = ["a", "b"]
                            , body = eap (eap (evar "+") (evar "a")) (evar "b")
                            , annot = Just $ toScheme tIntBinop
                            }]]
        expectedEnv = TypeEnv $ Map.fromList
                [ ("+", toScheme tIntBinop)
                , ("f", toScheme tIntBinop)
                ]

testExplicitBindingFail :: Assertion
testExplicitBindingFail = tiProgram defaultEnv program @?= err
  where
    program = [[Binding { identifier = "f"
                        , arguments = ["a", "b"]
                        , body = eap (eap (evar "+") (evar "a")) (evar "b")
                        , annot = t
                        }]]
    t = Just $ fromMaybe (toScheme tUnit) $ parseType "a -> a -> a"
    err = Left $ InferenceError "signature too general"

testAnnotatePrograms :: Assertion
testAnnotatePrograms =
    fmap third (tiProgram defaultEnv program) @?= Right expectedProgram
      where
        third (_, _, x) = x
        binding = Binding { identifier = "f"
                          , arguments = ["x"]
                          , body = e
                          , annot = Nothing
                          }
        program = [[binding]]
        e = eap (eap (evar "+") (evar "x")) (elit (LInt 1))
        annotatedE = tInt :< EAp (tIntUnop :< EAp (tIntBinop :< EVar "+")
                (tInt :< EVar "x")) (tInt :< ELit (LInt 1))
        expectedProgram = [[binding { body = annotatedE
                                    , annot = toScheme tIntUnop
                                    }]]

tests :: TestTree
tests = testGroup "Lang.TypeInference"
    [ testCase "Expr EVar" testTiExprId
    , testCase "Large Binding Group" testLargeBindingGroup
    , testCase "Minimal Binding Group" testMinimalBindingGroup
    , testCase "Explicit Binding" testExplicitBinding
    , testCase "Explicit Binding Fail" testExplicitBindingFail
    , testCase "Annotate Programs" testAnnotatePrograms
    ]

