module Lang.Expr where

import Control.Comonad.Cofree

import Lang.Core
import Lang.Identifier
import Lang.Type

type Expr a b = Cofree (ExprF a) b

evar :: VariableName -> Expr a ()
evar = (() :<) . EVar

elit :: Literal -> Expr a ()
elit = (() :<) . ELit

eap :: Expr a () -> Expr a () -> Expr a ()
eap e1 e2 = () :< EAp e1 e2

elet :: BindingGroup a (Expr a ()) -> Expr a () -> Expr a ()
elet bg e = () :< ELet bg e

type SyntacticExpr = Expr (Maybe Scheme) ()

