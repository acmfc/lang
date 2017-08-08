module Lang.Core where

import Control.Comonad.Cofree

import Lang.Identifier

-- | An expression where every binding is annotated with a.
data ExprF a b = EVar VariableName
    | ELit Literal
    | EAp b b
    | ELet (BindingGroup a b) b
    deriving (Show, Eq)

type Expr a b = Cofree (ExprF a) b

-- | A single variable or function binding.
data Binding a b = Binding { identifier :: VariableName
                           , arguments :: [VariableName]
                           , body :: b
                           , annot :: a
                           } deriving (Show, Eq)

-- | A minimal set of mutually recursive bindings, i.e. every Binding in a
-- BindingGroup depends on every other Binding in the group.
type BindingGroup a b = [Binding a b]

type Program a b = [BindingGroup a b]

data Literal = LInt Integer | LLab LabelName
    deriving (Show, Eq)

