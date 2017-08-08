module Lang.TypeInference.Tests ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import Debug.Trace
import Text.Parsec (parse)
import qualified Control.Exception.Base as Base
import qualified Data.Map as Map

import Lang.Core
import Lang.Type
import Lang.TypeInference
import qualified Lang.Parser as Parser

tvs :: [Tyvar]
tvs = map (\n -> Tyvar ("t" ++ show n) KStar) ([0..] :: [Integer])

parseTypePartial :: String -> Scheme
parseTypePartial s = case parse Parser.typ "" s of
    Left _ -> Base.assert False undefined
    Right scheme -> scheme

-- | Define an environment for use by tests containing basic operations.
defaultEnv :: TypeEnv
defaultEnv = TypeEnv (Map.fromList [("+", toScheme t)])
  where
    t = makeFun tInt (makeFun tInt tInt)

testTiExprId :: Assertion
testTiExprId = runTI (tiExpr defaultEnv e) @?= ([], expectedType)
  where
    e = ELet [idBinding] (EVar "id")
    idBinding = Binding { identifier = "id", arguments = ["a"], body = EVar "a", annot = Nothing }
    expectedType = makeFun (TVar (tvs !! 2)) (TVar (tvs !! 2))

-- | Check that binding groups containing more than a minimal set of mutually
-- recursive definitions will unnecessarily restrict polymorphism.
testLargeBindingGroup :: Assertion
testLargeBindingGroup = snd (tiProgram defaultEnv program) @?= expectedEnv
  where
    expectedEnv = TypeEnv (Map.fromList
                  [ ("+", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("f", toScheme $ foldr makeFun tInt [tInt, tInt])
                  , ("id", toScheme $ makeFun tInt tInt)
                  ])
    program = [[ Binding { identifier = "id", arguments = ["a"], body = EVar "a", annot = Nothing}
               , Binding { identifier = "f", arguments = fArgs, body = fBody, annot = Nothing}
               ]]
    fArgs = ["a", "b"]
    fBody = EAp (EAp (EVar "+") (EVar "a")) (EAp (EVar "id") (EVar "b"))

-- | Check that properly minimized binding groups result in maximally
-- polymorphic types.
testMinimalBindingGroup :: Assertion
testMinimalBindingGroup = snd (tiProgram defaultEnv program) @?= expectedEnv
  where
    expectedEnv = TypeEnv $ Map.fromList
            [ ("+", toScheme $ foldr makeFun tInt [tInt, tInt])
            , ("f", toScheme $ foldr makeFun tInt [tInt, tInt])
            , ("id", Scheme [tvs !! 1] (Qual [] tId))
            ]
    tId = makeFun (TVar (tvs !! 1)) (TVar (tvs !! 1))
    program = [ [Binding { identifier = "id", arguments = ["a"], body = EVar "a", annot = Nothing}]
              , [Binding { identifier = "f", arguments = fArgs, body = fBody, annot = Nothing}]
              ]
    fArgs = ["a", "b"]
    fBody = EAp (EAp (EVar "+") (EVar "a")) (EAp (EVar "id") (EVar "b"))

testExplicitBinding :: Assertion
testExplicitBinding = snd (tiProgram defaultEnv program) @?= expectedEnv
  where
    program = [[Binding { identifier = "f"
                       , arguments = ["a", "b"]
                       , body = EAp (EAp (EVar "+") (EVar "a")) (EVar "b")
                       , annot = Just $ toScheme $ foldr makeFun tInt [tInt, tInt]
                       }]]
    expectedEnv = TypeEnv $ Map.fromList
            [ ("+", toScheme $ foldr makeFun tInt [tInt, tInt])
            , ("f", toScheme $ foldr makeFun tInt [tInt, tInt])
            ]

testExplicitBindingFail :: Assertion
testExplicitBindingFail = trace (show ps) env @?= expectedEnv
  where
    (ps, env) = tiProgram defaultEnv program
    program = [[Binding { identifier = "f"
                        , arguments = ["a", "b"]
                        , body = EAp (EAp (EVar "+") (EVar "a")) (EVar "b")
                        , annot = Just $ parseTypePartial "a -> a -> a"
                        }]]
    expectedEnv = TypeEnv $ Map.fromList
            [ ("+", toScheme $ foldr makeFun tInt [tInt, tInt])
            , ("f", toScheme tInt)
            ]

tests :: TestTree
tests = testGroup "Lang.TypeInference"
    [ testCase "Expr EVar" testTiExprId
    , testCase "Large Binding Group" testLargeBindingGroup
    , testCase "Minimal Binding Group" testMinimalBindingGroup
    , testCase "Explicit Binding" testExplicitBinding
    , testCase "Explicit Binding Fail" testExplicitBindingFail
    ]

