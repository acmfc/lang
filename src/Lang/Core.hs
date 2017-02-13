module Lang.Core where

type VariableName = String
data Expr = EVar VariableName
    | ELit Literal
    | EAp Expr Expr
    | ELet BindingGroup Expr
    deriving (Show)

-- | Represent a single variable or function binding.
data Binding = Binding { identifier :: VariableName
                       , arguments :: [VariableName]
                       , body :: Expr
                       } deriving (Show)
-- | Represent a set of mutually recursive bindings.
type BindingGroup = [Binding]

data Literal = LInt Integer
    deriving (Show)
