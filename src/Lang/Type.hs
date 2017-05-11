{-|
Implements a simple Hindley-Milner type system. The type representations and
unification code are based on:
<https://web.cecs.pdx.edu/~mpj/thih/ Typing Haskell in Haskell>.
-}
module Lang.Type where

import Control.Exception.Base (assert)
import Control.Monad (zipWithM, ap)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lang.Core

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

tInt :: Type
tInt = TCon (Tycon "Int" KStar)

tArrow :: Type
tArrow = TCon (Tycon "(->)" (KFun KStar (KFun KStar KStar)))

makeFun :: Type -> Type -> Type
makeFun a b = TAp (TAp tArrow a) b

tRecordCon :: Type
tRecordCon = TCon (Tycon "{_}" (KFun KRow KStar))

tVariantCon :: Type
tVariantCon = TCon (Tycon "<_>" (KFun KRow KStar))

tLabelCon :: Type
tLabelCon = TCon (Tycon "Lab" (KFun KLab KStar))

makeLabel :: VariableName -> Type
makeLabel l = TAp tLabelCon (TCon (Tycon l KLab))

-- Notes from fclabels:
-- row variable: TVar (Tyvar "rowvarname" KRow)
-- label constant: TCon (Tycon "labelname" KLab)
-- label variable: TVar (Tyvar "labelvarname" KLab)
-- row expressions r: RVar Tyvar | REmpty | RExt Type Type r
-- kind r = KRow
-- syntacticLabelEq (TCon (Tycon name1 KLab)) (TCon (Tycon name2 KLab)) | name1 == name2 = True
-- syntacticLabelEq _ _ = False
--
-- l \neq_c l^\prime
-- constantLabelsDistinct (TCon (Tycon name1 KLab)) (TCon (Tycon name2 KLab)) | name1 == name2 = True
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
    kind (TAp t _) = case (kind t) of
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
    ftv xs = foldr Set.union Set.empty (map ftv xs)
    apply sub = map (apply sub)

-- | Create a left-biased composition of two substitutions.
-- prop> apply (compose sub1 sub2) == (apply sub1 . apply sub2)
compose :: Subst -> Subst -> Subst
compose sub1 sub2 = Map.map (apply sub1) sub2 `Map.union` sub1

singleton :: Tyvar -> Type -> Subst
singleton = Map.singleton

empty :: Subst
empty = Map.empty

-- | Compute the most general unifier of two types.
unifier :: Monad m => Type -> Type -> m Subst
unifier (TAp t1 t2) (TAp t1' t2') = do
    sub1 <- unifier t1 t1'
    sub2 <- unifier (apply sub1 t2) (apply sub1 t2')
    return $ compose sub2 sub1
unifier (TVar u) t = varBind u t
unifier t (TVar u) = varBind u t
unifier t1 t2
    | t1 == t2 = return empty
    | otherwise = fail $ "Types do not unify: " ++ show t1 ++ ", " ++ show t2

-- | Create a substitution mapping a Tyvar to a Type after performing an occurs
-- check.
varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u (TVar v) | u == v = return empty
varBind u t
    | u `elem` typeVariables t =
        fail $ "Occurs check failed: " ++ show u ++ " occurs in " ++ show t
    | kind u /= kind t =
        fail $ concat [ "Kinds do not match: "
                      , show u, " has kind ", show (kind u), ", "
                      , show t, " has kind ", show (kind t)
                      ]
    | otherwise = return $ singleton u t

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

instance Types Scheme where
    apply sub (Scheme tvs qt) =
        Scheme tvs (apply (foldr Map.delete sub tvs) qt)
    ftv (Scheme tvs t) = Set.difference (ftv t) (Set.fromList tvs)

-- | Map term variables to type schemes.
newtype TypeEnv = TypeEnv (Map.Map VariableName Scheme)
    deriving (Show, Eq)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply sub (TypeEnv env) = TypeEnv (Map.map (apply sub) env)

-- | Define a monad to track the state of the substitution and a counter for
-- generating type variable names.
newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Functor TI where
    fmap f (TI g) = TI (\s n -> let (s', m, x) = g s n in (s', m, f x))

instance Applicative TI where
    pure = return
    (<*>) = ap

-- TODO Implement fail.
instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
        (s', m, x) -> let TI gx = g x in gx s' m)

runTI :: TI a -> a
runTI (TI f) = x where (_, _, x) = f empty 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

extendSubst :: Subst -> TI ()
extendSubst s = TI (\s' n -> (compose s s', n, ()))

unify :: Type -> Type -> TI ()
unify t1 t2 = do sub <- getSubst
                 u <- unifier (apply sub t1) (apply sub t2)
                 extendSubst u

newTVar :: Kind -> TI Type
newTVar k = TI (\s n -> (s, n + 1, TVar (Tyvar ("t" ++ show n) k)))

-- | Quantify all type variables not free in the environment.
gen :: TypeEnv -> Qual Type -> Scheme
gen env qt = Scheme tvs qt
  where tvs = Set.toList (Set.difference (ftv qt) (ftv env))

-- | Instantiate quantified type variables in a type scheme with fresh type
-- variables.
inst :: Scheme -> TI (Qual Type)
inst (Scheme tvs qt) = do ts <- mapM newTVar (map kind tvs)
                          return $ apply (Map.fromList (zip tvs ts)) qt

type Infer e t = TypeEnv -> e -> TI ([Pred], t)

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LInt _) = return ([], tInt)
tiLit (LLab l) = return ([], makeLabel l)

tiExpr :: Infer Expr Type
tiExpr (TypeEnv env) (EVar x) = case Map.lookup x env of
    Nothing -> fail $ "Unbound variable: " ++ x
    Just scheme -> do Qual ps t <- inst scheme
                      return (ps, t)
tiExpr _ (ELit l) = tiLit l
tiExpr env (EAp e1 e2) = do
    (ps, te1) <- tiExpr env e1
    (qs, te2) <- tiExpr env e2
    t <- newTVar KStar
    unify (makeFun te2 t) te1
    return (ps ++ qs, t)
tiExpr (TypeEnv env) (ELet bg e) = do
    (ps, TypeEnv env') <- tiBindingGroup (TypeEnv env) bg
    (qs, t) <- tiExpr (TypeEnv (Map.union env' env)) e
    return (ps ++ qs, t)

-- | Creates a polytype from a monotype without any quantifiers.
toScheme :: Type -> Scheme
toScheme = Scheme [] . Qual []

-- | Run type inference for the expression in a binding and unify it with t.
-- Because t takes on the type of the expression, env should map b's identifier
-- to t to allow for recursive bindings.
tiBoundExpr :: TypeEnv -> Binding -> Type -> TI [Pred]
tiBoundExpr (TypeEnv env) b t = do
    -- Create a fresh type variable for each argument.
    targs <- mapM (const (newTVar KStar)) (arguments b)
    let schemes = map toScheme targs
        env' = Map.union (Map.fromList (zip (arguments b) schemes)) env
    (ps, te) <- tiExpr (TypeEnv env') (body b)
    unify t $ foldr makeFun te targs
    return ps

-- | Run type inference for each binding in the binding group. Every expression
-- within the group can see the instantiated type variables representing every
-- other. In order to obtain the most general types, bg should be a minimal set
-- of mutually recursive definitions.
-- TODO Split predicates into deferred and retained predicates.
tiBindingGroup :: Infer BindingGroup TypeEnv
tiBindingGroup (TypeEnv env) bg = do
    -- Allocate a type variable for each binding.
    ts <- mapM (const (newTVar KStar)) bg
    let identifiers = map identifier bg
        schemes = map toScheme ts
        -- Create an environment mapping each identifier to its type scheme.
        env' = Map.union (Map.fromList (zip identifiers schemes)) env
    -- Unify the type allocated for each binding with the type inferred for its
    -- expression.
    pss <- zipWithM (tiBoundExpr (TypeEnv env')) bg ts
    sub <- getSubst
    let ps' = apply sub (concat pss)
        ts' = apply sub ts
        -- Generalize all the types we found for the newly-bound names.
        schemes' = map (gen (apply sub (TypeEnv env)) . Qual ps') ts'
    return (ps', TypeEnv . Map.fromList $ zip identifiers schemes')

-- | Enhance a type inferencer to run over a list rather than a single entity,
-- accumulating an environment as it goes.
tiSequence :: Infer a TypeEnv -> Infer [a] TypeEnv
tiSequence _ env [] = return ([], env)
tiSequence ti (TypeEnv env) (x : xs) = do
    (ps, TypeEnv env') <- ti (TypeEnv env) x
    (qs, TypeEnv env'') <- tiSequence ti (TypeEnv (Map.union env' env)) xs
    return (ps ++ qs, TypeEnv $ Map.union env'' env')

type Program = [BindingGroup]

tiProgram :: TypeEnv -> Program -> TypeEnv
tiProgram env bgs = runTI $ do
    (_, env') <- tiSequence tiBindingGroup env bgs
    s <- getSubst
    return $ apply s env'

