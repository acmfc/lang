module Lang.TypeInference where

import Control.Comonad.Cofree
import Control.Monad (zipWithM, ap)
import Control.Monad.State
import Data.Either (partitionEithers)
import qualified Data.Map as Map

import Lang.Core
import Lang.Type

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

-- TODO Use a distinctive symbol so the pretty printer can determine whether a
-- type variable was provided by a user annotation. Alternatively make type
-- variable names a sum type.
newTVar :: Kind -> TI Type
newTVar k = TI (\s n -> (s, n + 1, TVar (Tyvar ("t" ++ show n) k)))

-- | Instantiate quantified type variables in a type scheme with fresh type
-- variables.
inst :: Scheme -> TI (Qual Type)
inst (Scheme tvs qt) = do ts <- mapM (newTVar . kind) tvs
                          return $ apply (Map.fromList (zip tvs ts)) qt

type Infer e t = TypeEnv -> e -> TI ([Pred], t)

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LInt _) = return ([], tInt)
tiLit (LLab l) = return ([], makeLabelConst l)

tiExpr :: Infer (Expr (Maybe Scheme) ()) Type
tiExpr (TypeEnv env) (_ :< EVar x) = case Map.lookup x env of
    Nothing -> fail $ "Unbound variable: " ++ x
    Just scheme -> do Qual ps t <- inst scheme
                      return (ps, t)
tiExpr _ (_ :< ELit l) = tiLit l
tiExpr env (_ :< EAp e1 e2) = do
    (ps, te1) <- tiExpr env e1
    (qs, te2) <- tiExpr env e2
    t <- newTVar KStar
    unify (makeFun te2 t) te1
    return (ps ++ qs, t)
tiExpr (TypeEnv env) (_ :< ELet bg e) = do
    (ps, TypeEnv env') <- tiBindingGroup (TypeEnv env) bg
    (qs, t) <- tiExpr (TypeEnv (Map.union env' env)) e
    return (ps ++ qs, t)

-- | Run type inference for the expression in a binding and unify it with t.
-- Because t takes on the type of the expression, env should map b's identifier
-- to t to allow for recursive bindings.
tiBoundExpr :: TypeEnv -> Binding a (Expr (Maybe Scheme) ()) -> Type -> TI [Pred]
tiBoundExpr (TypeEnv env) b t = do
    -- Create a fresh type variable for each argument.
    targs <- mapM (const (newTVar KStar)) (arguments b)
    let schemes = map toScheme targs
        env' = Map.union (Map.fromList (zip (arguments b) schemes)) env
    (ps, te) <- tiExpr (TypeEnv env') (body b)
    unify t $ foldr makeFun te targs
    return ps

equivalentTypes :: Type -> Type -> State Subst Bool
equivalentTypes (TVar tyv1) (TVar tyv2) = do
    sub <- get
    case Map.lookup tyv1 sub of
        Just t -> return $ t == TVar tyv2
        Nothing -> do
            put $ Map.insert tyv1 (TVar tyv2) sub
            return True
equivalentTypes (TAp t1 t2) (TAp t1' t2') = do
    b1 <- equivalentTypes t1 t1'
    b2 <- equivalentTypes t2 t2'
    return $ b1 && b2
equivalentTypes t1 t2 = return $ t1 == t2

equivalentRows :: Row -> Row -> State Subst Bool
equivalentRows (RVar t1) (RVar t2) = equivalentTypes t1 t2
equivalentRows (RExt t1 t2 r) (RExt t1' t2' r') = do
    b1 <- equivalentTypes t1 t1'
    b2 <- equivalentTypes t2 t2'
    b3 <- equivalentRows r r'
    return $ b1 && b2 && b3
equivalentRows r1 r2 = return $ r1 == r2

equivalentPreds :: Pred -> Pred -> State Subst Bool
equivalentPreds (RowLacks r1 t1) (RowLacks r2 t2) = do
    b1 <- equivalentRows r1 r2
    b2 <- equivalentTypes t1 t2
    return $ b1 && b2
equivalentPreds (RowEq r1 r2) (RowEq r1' r2') = do
    b1 <- equivalentRows r1 r1'
    b2 <- equivalentRows r2 r2'
    return $ b1 && b2
equivalentPreds _ _ = return False

equivalentQuals :: Qual Type -> Qual Type -> State Subst Bool
equivalentQuals (Qual ps1 t1) (Qual ps2 t2) = do
    b1s <- zipWithM equivalentPreds ps1 ps2
    b2 <- equivalentTypes t1 t2
    return $ and b1s && b2

equivalentTyvars :: Tyvar -> Tyvar -> State Subst Bool
equivalentTyvars tyv1 tyv2 = do
    sub <- get
    case Map.lookup tyv1 sub of
        Just (TVar tyv2') -> return $ tyv2 == tyv2'
        Just _ -> return False
        Nothing -> do
            put $ Map.insert tyv1 (TVar tyv2) sub
            return True

-- | Returns True if there is a substitution mapping the first argument to the
-- second.
equivalentSchemes :: Scheme -> Scheme -> State Subst Bool
equivalentSchemes (Scheme tyvs1 qt1) (Scheme tyvs2 qt2) = do
    b1s <- zipWithM equivalentTyvars tyvs1 tyvs2
    b2 <- equivalentQuals qt1 qt2
    return $ and b1s && b2

-- TODO Split predicates into deferred and retained predicates.
tiExplBinding :: TypeEnv -> Binding Scheme (Expr (Maybe Scheme) ()) -> TI [Pred]
tiExplBinding (TypeEnv env) b = do
    let schemeAnnot = annot b
    Qual qs t <- inst schemeAnnot
    ps <- tiBoundExpr (TypeEnv env) b t
    sub <- getSubst
    let qs' = apply sub qs
        t' = apply sub t
        schemeInferred = gen (apply sub (TypeEnv env)) (Qual qs' t')
    let (equivalent, _) = runState (equivalentSchemes schemeInferred schemeAnnot) empty
    if not equivalent then fail "signature too general"
    else return $ apply sub ps

-- | Run type inference for each binding in the binding group. Every expression
-- within the group can see the instantiated type variables representing every
-- other.
-- TODO Split predicates into deferred and retained predicates.
-- TODO Implement the monomorphism restriction.
tiImplBindingGroup :: Infer (BindingGroup () (Expr (Maybe Scheme) ())) TypeEnv
tiImplBindingGroup (TypeEnv env) bg = do
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

tiBindingGroup :: Infer (BindingGroup (Maybe Scheme) (Expr (Maybe Scheme) ())) TypeEnv
tiBindingGroup (TypeEnv env) bg = do
    let (es, is) = partitionByAnnotation bg
        TypeEnv env' = envFromExplBindingGroup es
    (ps, TypeEnv env'') <- tiImplBindingGroup (TypeEnv $ Map.union env' env) is
    let fullEnv = TypeEnv $ env'' `Map.union` env' `Map.union` env
    qss <- mapM (tiExplBinding fullEnv) es
    return (ps ++ concat qss, TypeEnv $ Map.union env'' env')

envFromExplBindingGroup :: BindingGroup Scheme (Expr (Maybe Scheme) ()) -> TypeEnv
envFromExplBindingGroup bg = TypeEnv $ foldr addAnnot Map.empty annotPairs
  where
    addAnnot (v, schema) = Map.insert v schema
    annotPairs =
        [(v, schema) | Binding { identifier = v, annot = schema } <- bg]

partitionByAnnotation :: BindingGroup (Maybe Scheme) (Expr (Maybe Scheme) ())
                      -> ( BindingGroup Scheme (Expr (Maybe Scheme) ())
                         , BindingGroup () (Expr (Maybe Scheme) ())
                         )
partitionByAnnotation = partitionEithers . map f
  where
    f x = case annot x of Just sc -> Left (x { annot = sc })
                          Nothing -> Right (x { annot = () })

-- | Enhance a type inferencer to run over a list rather than a single entity,
-- accumulating an environment as it goes.
tiSequence :: Infer a TypeEnv -> Infer [a] TypeEnv
tiSequence _ env [] = return ([], env)
tiSequence ti (TypeEnv env) (x : xs) = do
    (ps, TypeEnv env') <- ti (TypeEnv env) x
    (qs, TypeEnv env'') <- tiSequence ti (TypeEnv (Map.union env' env)) xs
    return (ps ++ qs, TypeEnv $ Map.union env'' env')

-- TODO Deal with ambiguities and defaults.
tiProgram :: TypeEnv -> Program (Maybe Scheme) (Expr (Maybe Scheme) ()) -> ([Pred], TypeEnv)
tiProgram env bgs = runTI $ do
    (ps, env') <- tiSequence tiBindingGroup env bgs
    s <- getSubst
    return (ps, apply s env')

