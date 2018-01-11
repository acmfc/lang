{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lang.Expr where

import Control.Comonad.Cofree
import Data.Set (union)

import Lang.Core
import Lang.Identifier
import Lang.Type

type Expr a b = Cofree (ExprF a) b

instance (Types a, Types b) => Types (Expr a b) where
    ftv (t :< EAp e1 e2) = ftv t `union` ftv e1 `union` ftv e2
    ftv (t :< ELet bg e) = ftv t `union` ftv bg `union` ftv e
    ftv (t :< _) = ftv t
    apply s (t :< EAp e1 e2) = apply s t :< EAp (apply s e1) (apply s e2)
    apply s (t :< ELet bg e) = apply s t :< ELet (apply s bg) (apply s e)
    apply s (t :< e) = apply s t :< e

-- TODO Make a decision on whether to allow orphan instances.
--instance (Types a, Types b) => Types (WrapExpr a b) where
--    ftv (WrapExpr (t :< EAp e1 e2)) =
--        ftv t `union` ftv (WrapExpr e1) `union` ftv (WrapExpr e2)
--    ftv (WrapExpr (t :< ELet bg e)) =
--        ftv t `union` ftv bg `union` ftv (WrapExpr e)
--    ftv (WrapExpr (t :< _)) = ftv t
--    apply s (WrapExpr (t :< EAp e1 e2)) =
--        WrapExpr $ apply s t :< EAp (apply s e1) (apply s e2)
--    apply s (WrapExpr (t :< ELet bg e)) =
--        WrapExpr $ apply s t :< ELet (apply s bg) (apply s e)
--    apply s (WrapExpr (t :< e)) = WrapExpr $ apply s t :< e

instance (Types a, Types b) => Types (Binding a b) where
    ftv b = ftv (annot b) `union` ftv (body b)
    apply s b = b { annot = apply s (annot b), body = apply s (body b) }

evar :: VariableName -> Expr a ()
evar = (() :<) . EVar

elit :: Literal -> Expr a ()
elit = (() :<) . ELit

eap :: Expr a () -> Expr a () -> Expr a ()
eap e1 e2 = () :< EAp e1 e2

elet :: BindingGroup a (Expr a ()) -> Expr a () -> Expr a ()
elet bg e = () :< ELet bg e

type SyntacticExpr = Expr (Maybe Scheme) ()

type TypedExpr = Expr Scheme Type
type TypedBinding = Binding Scheme TypedExpr
type TypedBindingGroup = BindingGroup Scheme TypedExpr
type TypedProgram = Program Scheme TypedExpr

