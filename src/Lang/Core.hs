module Lang.Core where

type VariableName = String
data Expr = EVar VariableName
  | ELam VariableName Expr
  | EAp Expr Expr
  | ELetrec [VariableName] [Expr] Expr
  deriving (Show)

