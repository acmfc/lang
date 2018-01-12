module Main where

import Data.List
import Data.Maybe (fromJust)
import Text.PrettyPrint

import qualified Data.Map as Map

import Lang.DependencyAnalysis
import Lang.Parser
import Lang.PrettyPrint
import Lang.Type
import Lang.TypeInference

makeTypeEnv :: [(String, String)] -> Maybe TypeEnv
makeTypeEnv spec = fmap (TypeEnv . Map.fromList) $ foldr f (Just []) parsedSpec
  where
    f (name, Right typeSpec) (Just acc) = Just ((name, typeSpec) : acc)
    f _ _ = Nothing
    parsedSpec = map (\(name, typeSpec) -> (name, parseTyp typeSpec)) spec

initialTypeEnv :: TypeEnv
initialTypeEnv = fromJust . makeTypeEnv $ spec
  where
    spec = [ ("add", "Int -> Int -> Int")
           , ("select", "Lab l -> {l : a} -> a")
           ]

exampleProgram :: String
exampleProgram = intercalate "\n"
    [ "let apply f x = f x"
    , "let f a = add a 0"
    , "let incx r = add 1 (select @x r)"
    , "val z: a -> (a -> c) -> c"
    , "let z x f = f x"
    ]

main :: IO ()
main = case parseProgram exampleProgram of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> case tiProgram initialTypeEnv (structureBindings ast) of
        Left err -> putStrLn $ "Type inference error: " ++ show err
        Right (_, typeEnv, typedProgram) -> do
            putStrLn "Demo program was successfully type-checked."
            putStrLn "Type environment:"
            mapM_ putStrLn $ printTypeEnv typeEnv
            putStrLn "\nTyped program:"
            putStrLn . render $ prettyPrint typedProgram

