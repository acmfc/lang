module Lang.Core where

type VariableName = String

data Expr = EVar VariableName
    | ELit Literal
    | EAp Expr Expr
    | ELet BindingGroup Expr
    deriving (Show, Eq)

-- | Represents a single variable or function binding.
data Binding = Binding { identifier :: VariableName
                       , arguments :: [VariableName]
                       , body :: Expr
                       } deriving (Show, Eq)

-- | Represents a minimal set of mutually recursive bindings, i.e. every
-- Binding in a BindingGroup depends on every other Binding in the group.
type BindingGroup = [Binding]

data Literal = LInt Integer | LLab VariableName
    deriving (Show, Eq)
