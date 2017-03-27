module Main where

import Data.List
import qualified Data.Map as Map

import Lang.DependencyAnalysis
import Lang.PrettyPrint
import Lang.Type
import Lang.Parser

initialTypeEnv :: TypeEnv
initialTypeEnv =
    TypeEnv $ Map.fromList [("add", toScheme (TFun TInt (TFun TInt TInt)))]

exampleProgram :: String
exampleProgram = intercalate "\n"
    [ "let f a = add a (g 0)"
    , "let g a = h a"
    , "let h a = g a"
    ]

main :: IO ()
main = case parseProgram exampleProgram of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> do
        let typeEnv = tiProgram initialTypeEnv (structureBindings ast)
        putStrLn "Demo program was successfully type-checked:"
        mapM_ putStrLn $ printTypeEnv typeEnv

