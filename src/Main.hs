module Main where

import Data.List
import qualified Data.Map as Map

import Lang.DependencyAnalysis
import Lang.Parser
import Lang.PrettyPrint
import Lang.Type
import Lang.TypeInference

initialTypeEnv :: TypeEnv
initialTypeEnv =
    TypeEnv $ Map.fromList
        [ ("add", toScheme tAdd)
        , ("select", gen (TypeEnv Map.empty) qtSelect)
        ]
      where
        tAdd = makeFun tInt (makeFun tInt tInt)
        tSelect = makeFun targ1 (makeFun targ2 (TVar (Tyvar "u0" KStar)))
          where
            targ1 = TAp tLabelCon (TVar (Tyvar "l" KLab))
            targ2 = TAp tRecordCon (TVar (Tyvar "r1" KRow))
        qtSelect = Qual [ RowLacks (RVar (TVar (Tyvar "r2" KRow))) (TVar (Tyvar "l" KLab))
                        , RowEq (RVar (TVar (Tyvar "r1" KRow))) (RExt (TVar (Tyvar "l" KLab)) (TVar (Tyvar "u0" KStar)) (RVar (TVar (Tyvar "r2" KRow))))
                        ] tSelect

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
    --Right ast -> putStrLn $ show ast
    Right ast -> do
        let (_, typeEnv) = tiProgram initialTypeEnv (structureBindings ast)
        putStrLn "Demo program was successfully type-checked:"
        mapM_ putStrLn $ printTypeEnv typeEnv

