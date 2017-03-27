module Main where

import qualified Data.Map as Map

import Lang.Core
import Lang.DependencyAnalysis
import Lang.PrettyPrint
import Lang.Type
import Lang.Parser

initialTypeEnv :: TypeEnv
initialTypeEnv =
    TypeEnv $ Map.fromList [("+", toScheme (TFun TInt (TFun TInt TInt)))]

exampleProgram :: Program
exampleProgram = structureBindings
    [ Binding { identifier="f", arguments=["a"],
        body=EAp (EAp (EVar "+") (EVar "a")) (EAp (EVar "g") (ELit (LInt 0))) }
    , Binding { identifier="g", arguments=["a"],
        body=EAp (EVar "h") (EVar "a") }
    , Binding { identifier="h", arguments=["a"],
        body=EAp (EVar "g") (EVar "a") }
    ]

main :: IO ()
main = do
    let typeEnv = tiProgram initialTypeEnv exampleProgram
    putStrLn "Demo program was successfully type-checked:"
    mapM_ putStrLn $ printTypeEnv typeEnv
