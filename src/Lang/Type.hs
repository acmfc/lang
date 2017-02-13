{-|
This module implements a simple Hindley-Milner type system. The type
representations and unification algorithm are based on
<https://web.cecs.pdx.edu/~mpj/thih/ Typing Haskell in Haskell>.
-}
module Lang.Type where

import Control.Monad (zipWithM_, ap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lang.Core

type TypeVariableName = String
data Type = TVar TypeVariableName
    | TInt
    | TFun Type Type
    deriving (Show, Eq, Ord)

-- | Collect all TypeVariableNames referenced in a Type.
typeVariables :: Type -> Set.Set TypeVariableName
typeVariables (TVar tvn) = Set.singleton tvn
typeVariables TInt = Set.empty
typeVariables (TFun t1 t2) = Set.union (typeVariables t1) (typeVariables t2)

type Subst = Map.Map TypeVariableName Type

class Types a where
    ftv :: a -> Set.Set TypeVariableName
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVar tvn) = Set.singleton tvn
    ftv TInt = Set.empty
    ftv (TFun t1 t2) = Set.union (ftv t1) (ftv t2)
    apply sub (TVar tvn) = Map.findWithDefault (TVar tvn) tvn sub
    apply sub (TFun t1 t2) = TFun (apply sub t1) (apply sub t2)
    apply _ t = t

-- | Define a type scheme or polytype. Contains a type and a list of all
-- quantified type variables within the type.
data Scheme = Scheme [TypeVariableName] Type
    deriving (Show, Eq)

instance Types Scheme where
    ftv (Scheme tvns t) = Set.difference (ftv t) (Set.fromList tvns)
    apply sub (Scheme tvns t) =
        Scheme tvns (apply (foldr Map.delete sub tvns) t)

instance Types a => Types [a] where
    ftv xs = foldr Set.union Set.empty (map ftv xs)
    apply sub = map (apply sub)

-- | Create a left-biased composition of two substitutions.
-- Invariant: apply (compose sub1 sub2) == (apply sub1 . apply sub2)
-- TODO Test the invariant with QuickCheck.
compose :: Subst -> Subst -> Subst
compose sub1 sub2 = Map.map (apply sub1) sub2 `Map.union` sub1

singleton :: TypeVariableName -> Type -> Subst
singleton = Map.singleton

empty :: Subst
empty = Map.empty

-- | Compute the most general unifier of two types.
unifier :: Monad m => Type -> Type -> m Subst
unifier (TFun t1 t2) (TFun t1' t2') = do
    sub1 <- unifier t1 t1'
    sub2 <- unifier (apply sub1 t2) (apply sub1 t2')
    return $ compose sub2 sub1
unifier (TVar tvn) t = varBind tvn t
unifier t (TVar tvn) = varBind tvn t
unifier t1 t2
    | t1 == t2 = return empty
    | otherwise = fail $ "Types do not unify: " ++ show t1 ++ ", " ++ show t2

-- | Create a substitution mapping tvn to t.
varBind :: Monad m => TypeVariableName -> Type -> m Subst
varBind tvn t
    | t == TVar tvn = return empty
    | tvn `elem` typeVariables t =
        fail $ "Occurs check failed: " ++ tvn ++ " occurs in " ++ show t
    | otherwise = return $ singleton tvn t

-- | Map program variables to type schemes.
newtype Environment = Environment (Map.Map VariableName Scheme)
    deriving (Show, Eq)

instance Types Environment where
    ftv (Environment env) = ftv (Map.elems env)
    apply sub (Environment env) = Environment (Map.map (apply sub) env)

-- | Define a monad to track the state of the substitution and a counter for
-- generating variable names.
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

newTVar :: TI Type
newTVar = TI (\s n -> (s, n + 1, TVar ("t" ++ show n)))

-- | Quantify all type variables not free in the environment.
gen :: Environment -> Type -> Scheme
gen env t = Scheme tvns t
  where tvns = Set.toList (Set.difference (ftv t) (ftv env))

-- | Instantiate quantified type variables in a type scheme with fresh type
-- variables.
inst :: Scheme -> TI Type
inst (Scheme tvns t) = do ts <- mapM (\_ -> newTVar) tvns
                          return $ apply (Map.fromList (zip tvns ts)) t

type Infer e t = Environment -> e -> TI t

tiLit :: Literal -> TI Type
tiLit (LInt _) = return TInt

tiExpr :: Infer Expr Type
tiExpr (Environment env) (EVar x) = case Map.lookup x env of
    Nothing -> fail $ "Unbound variable: " ++ x
    Just scheme -> inst scheme
tiExpr _ (ELit l) = tiLit l
tiExpr env (EAp e1 e2) = do
    te1 <- tiExpr env e1
    te2 <- tiExpr env e2
    t <- newTVar
    unify (TFun te2 t) te1
    return t
tiExpr (Environment env) (ELet bg e) = do
    Environment env' <- tiBindingGroup (Environment env) bg
    tiExpr (Environment (Map.union env' env)) e

toScheme :: Type -> Scheme
toScheme = Scheme []

-- | Run type inference for the expression in a binding and unify it with t.
tiBoundExpr :: Environment -> Binding -> Type -> TI ()
tiBoundExpr (Environment env) b t = do
    -- Create a fresh type variable for each argument.
    targs <- mapM (\_ -> newTVar) (arguments b)
    let schemes = map toScheme targs
        env' = Map.union (Map.fromList (zip (arguments b) schemes)) env
    te <- tiExpr (Environment env') (body b)
    unify t $ foldr TFun te targs

-- | Run type inference for each binding in the binding group. Every expression
-- within the group can see the instantiated type variables representing every
-- other. In order to obtain the most general types, bg should be a minimal set
-- of mutually recursive definitions.
tiBindingGroup :: Infer BindingGroup Environment
tiBindingGroup (Environment env) bg = do
    -- Allocate a type variable for each binding.
    ts <- mapM (\_ -> newTVar) bg
    let identifiers = map identifier bg
        schemes = map toScheme ts
        -- Create an environment mapping each identifier to its type scheme.
        env' = Map.union (Map.fromList (zip identifiers schemes)) env
    -- Unify the type allocated for each binding with the type inferred for its
    -- expression.
    zipWithM_ (tiBoundExpr (Environment env')) bg ts
    sub <- getSubst
    let ts' = apply sub ts
        -- Generalize all the types we found for the newly-bound names.
        schemes' = map (gen (apply sub (Environment env))) ts'
    return . Environment . Map.fromList $ zip identifiers schemes'

-- | Enhance a type inferencer to run over a list rather than a single entity,
-- accumulating an environment as it goes.
tiSequence :: Infer a Environment -> Infer [a] Environment
tiSequence _ env [] = return env
tiSequence ti (Environment env) (x : xs) = do
    Environment env' <- ti (Environment env) x
    Environment env'' <- tiSequence ti (Environment (Map.union env' env)) xs
    return . Environment $ Map.union env'' env'

type Program = [BindingGroup]

tiProgram :: Environment -> Program -> Environment
tiProgram env bgs = runTI $ do
    env' <- tiSequence tiBindingGroup env bgs
    s <- getSubst
    return $ apply s env'

