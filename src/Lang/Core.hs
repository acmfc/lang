module Lang.Core where

type VariableName = String
data Expr = EVar VariableName
    | ELit Literal
    | EAp Expr Expr
    | ELet BindingGroup Expr
    deriving (Show)

-- | Represents a single variable or function binding.
data Binding = Binding { identifier :: VariableName
                       , arguments :: [VariableName]
                       , body :: Expr
                       } deriving (Show)
-- | Represents a minimal set of mutually recursive bindings.
type BindingGroup = [Binding]

data Literal = LInt Integer
    deriving (Show)
