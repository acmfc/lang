{-|
Implements a simple Hindley-Milner type system. The type representations and
unification code are based on:
<https://web.cecs.pdx.edu/~mpj/thih/ Typing Haskell in Haskell>.
-}
module Lang.Type where

import Control.Exception.Base (assert)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lang.Identifier

type TypeVariableName = String

data Kind = KStar | KFun Kind Kind | KRow | KLab
    deriving (Show, Eq, Ord)

data Type = TVar Tyvar
    | TCon Tycon
    | TAp Type Type
    deriving (Show, Eq, Ord)

data Tyvar = Tyvar TypeVariableName Kind
    deriving (Show, Eq, Ord)

data Tycon = Tycon String Kind
    deriving (Show, Eq, Ord)

tUnit :: Type
tUnit = TCon (Tycon "Unit" KStar)

tInt :: Type
tInt = TCon (Tycon "Int" KStar)

tArrow :: Type
tArrow = TCon (Tycon "(->)" (KFun KStar (KFun KStar KStar)))

makeFun :: Type -> Type -> Type
makeFun a = TAp (TAp tArrow a)

tRecordCon :: Type
tRecordCon = TCon (Tycon "{_}" (KFun KRow KStar))

tVariantCon :: Type
tVariantCon = TCon (Tycon "<_>" (KFun KRow KStar))

tLabelCon :: Type
tLabelCon = TCon (Tycon "Lab" (KFun KLab KStar))

makeLabelType :: LabelName -> Type
makeLabelType l = TAp tLabelCon (TCon (Tycon l KLab))

makeLabelVarType :: LabelName -> Type
makeLabelVarType l = TAp tLabelCon (TVar (Tyvar l KLab))

-- Notes from fclabels:
-- row variable: TVar (Tyvar "rowvarname" KRow)
-- label constant: TCon (Tycon "labelname" KLab)
-- label variable: TVar (Tyvar "labelvarname" KLab)
-- row expressions r: RVar Tyvar | REmpty | RExt Type Type r
-- kind r = KRow
-- syntacticLabelEq (TCon (Tycon name1 KLab)) (TCon (Tycon name2 KLab)) |
--     name1 == name2 = True
-- syntacticLabelEq _ _ = False
--
-- l \neq_c l^\prime
-- constantLabelsDistinct (TCon (Tycon name1 KLab)) (TCon (Tycon name2 KLab)) |
--     name1 == name2 = True
-- constantLabelsDistinct _ _ = False

class HasKind t where
    kind :: t -> Kind

instance HasKind Tyvar where
    kind (Tyvar _ k) = k

instance HasKind Tycon where
    kind (Tycon _ k) = k

instance HasKind Type where
    kind (TCon tc) = kind tc
    kind (TVar u) = kind u
    kind (TAp t _) = case kind t of
        (KFun _ k) -> k
        KStar -> assert False KStar
        KRow -> assert False KRow
        KLab -> assert False KLab

-- | Collect all Tyvars referenced in a Type.
typeVariables :: Type -> Set.Set Tyvar
typeVariables (TVar u) = Set.singleton u
typeVariables (TAp t1 t2) = Set.union (typeVariables t1) (typeVariables t2)
typeVariables _ = Set.empty

type Subst = Map.Map Tyvar Type

class Types a where
    ftv :: a -> Set.Set Tyvar
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVar u) = Set.singleton u
    ftv (TAp t1 t2) = Set.union (ftv t1) (ftv t2)
    ftv _ = Set.empty
    apply sub t@(TVar u) = Map.findWithDefault t u sub
    apply sub (TAp t1 t2) = TAp (apply sub t1) (apply sub t2)
    apply _ t = t

instance Types a => Types [a] where
    ftv = foldr (Set.union . ftv) Set.empty
    apply sub = map (apply sub)

-- | Create a left-biased composition of two substitutions.
-- prop> apply (compose sub1 sub2) == (apply sub1 . apply sub2)
compose :: Subst -> Subst -> Subst
compose sub1 sub2 = Map.map (apply sub1) sub2 `Map.union` sub1

singleton :: Tyvar -> Type -> Subst
singleton = Map.singleton

empty :: Subst
empty = Map.empty

data Row = RVar Type | REmpty | RExt Type Type Row
    deriving (Show, Eq, Ord)

instance Types Row where
    apply s (RVar t) = RVar (apply s t)
    apply s (RExt t1 t2 r) = RExt (apply s t1) (apply s t2) (apply s r)
    apply _ REmpty = REmpty
    ftv (RVar t) = ftv t
    ftv REmpty = Set.empty
    ftv (RExt t1 t2 r) = ftv t1 `Set.union` ftv t2 `Set.union` ftv r

data Pred = RowLacks Row Type | RowEq Row Row
    deriving (Show, Eq, Ord)

instance Types Pred where
    apply s (RowLacks t1 t2) = RowLacks (apply s t1) (apply s t2)
    apply s (RowEq t1 t2) = RowEq (apply s t1) (apply s t2)
    ftv (RowLacks t1 t2) = Set.union (ftv t1) (ftv t2)
    ftv (RowEq t1 t2) = Set.union (ftv t1) (ftv t2)

data Qual t = Qual [Pred] t
    deriving (Show, Eq, Ord)

instance Types t => Types (Qual t) where
    apply s (Qual ps t) = Qual (apply s ps) (apply s t)
    ftv (Qual ps t) = Set.union (ftv ps) (ftv t)

--unifierPred :: Pred -> Pred -> m Subst
--unifierPred (Pred t1) (Pred t2) = unifier t1 t2

-- | Define a type scheme or polytype. Contains a type and a list of all
-- quantified type variables within the type.
data Scheme = Scheme [Tyvar] (Qual Type)
    deriving (Show, Eq)

-- | Creates a polytype from a monotype without any quantifiers.
toScheme :: Type -> Scheme
toScheme = Scheme [] . Qual []

instance Types Scheme where
    apply sub (Scheme tvs qt) =
        Scheme tvs (apply (foldr Map.delete sub tvs) qt)
    ftv (Scheme tvs t) = Set.difference (ftv t) (Set.fromList tvs)

-- | Quantify all type variables not free in the environment.
gen :: Types a => a -> Qual Type -> Scheme
gen env qt = Scheme tvs qt
  where tvs = Set.toList (Set.difference (ftv qt) (ftv env))

genEmptyEnv :: Qual Type -> Scheme
genEmptyEnv qt = Scheme (Set.toList $ ftv qt) qt

-- | Map term variables to type schemes.
newtype TypeEnv = TypeEnv (Map.Map VariableName Scheme)
    deriving (Show, Eq)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply sub (TypeEnv env) = TypeEnv (Map.map (apply sub) env)

