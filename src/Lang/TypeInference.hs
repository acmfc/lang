{-# LANGUAGE FlexibleContexts #-}

module Lang.TypeInference where

import Control.Comonad.Cofree
import Control.Monad (zipWithM)
import Control.Monad.State
import Control.Monad.Except
import Data.Either (partitionEithers)
import qualified Data.Map as Map

import Lang.Core
import Lang.Expr
import Lang.Type

-- | Compute the most general unifier of two types.
unifier :: MonadError InferenceError m => Type -> Type -> m Subst
unifier (TAp t1 t2) (TAp t1' t2') = do
    s1 <- unifier t1 t1'
    s2 <- unifier (apply s1 t2) (apply s1 t2')
    return $ compose s2 s1
unifier (TVar u) t = varBind u t
unifier t (TVar u) = varBind u t
unifier t1 t2
    | t1 == t2 = return empty
    | otherwise = throwError . InferenceError $ msg
      where
        msg = "Types do not unify: " ++ show t1 ++ ", " ++ show t2

-- | Create a substitution mapping a Tyvar to a Type after performing an occurs
-- check.
varBind :: MonadError InferenceError m => Tyvar -> Type -> m Subst
varBind u (TVar v) | u == v = return empty
varBind u t
    | u `elem` typeVariables t = throwError . InferenceError $ occursErr
    | kind u /= kind t = throwError . InferenceError $ kindErr
    | otherwise = return $ singleton u t
      where
        occursErr = "Occurs check failed: " ++ show u ++ " occurs in " ++ show t
        kindErr = concat [ "Kinds do not match: "
                         , show u, " has kind ", show (kind u), ", "
                         , show t, " has kind ", show (kind t)
                         ]

data InferenceState = InferenceState { sub :: Subst
                                     , counter :: Int
                                     }

newtype InferenceError = InferenceError String
    deriving Eq

instance Show InferenceError where
    show (InferenceError s) = s

type TI a = ExceptT InferenceError (State InferenceState) a

runTI :: TI a -> Either InferenceError a
runTI ti = evalState (runExceptT ti) initState
  where
    initState = InferenceState { sub = empty, counter = 0 }

getSubst :: TI Subst
getSubst = fmap sub get

extendSubst :: Subst -> TI ()
extendSubst s = do
     st <- get
     put $ st { sub = compose s $ sub st }

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- unifier (apply s t1) (apply s t2)
                 extendSubst u

-- TODO Use a distinctive symbol so the pretty printer can determine whether a
-- type variable was provided by a user annotation. Alternatively make type
-- variable names a sum type.
newTVar :: Kind -> TI Type
newTVar k = do
    st <- get
    let n = counter st
    put st { counter = n + 1 }
    return $ TVar (Tyvar ("$t" ++ show n) k)

-- | Instantiate quantified type variables in a type scheme with fresh type
-- variables.
inst :: Scheme -> TI (Qual Type)
inst (Scheme tvs qt) = do ts <- mapM (newTVar . kind) tvs
                          return $ apply (Map.fromList (zip tvs ts)) qt

type Infer e t = TypeEnv -> e -> TI ([Pred], t)

tiLit :: Literal -> TI Type
tiLit (LInt _) = return tInt
tiLit (LLab l) = return $ makeConstLabel l

tiExpr :: Infer SyntacticExpr TypedExpr
tiExpr (TypeEnv env) (_ :< EVar x) = case Map.lookup x env of
    Nothing -> throwError . InferenceError $ "Unbound variable: " ++ x
    Just scheme -> do Qual ps t <- inst scheme
                      return (ps, t :< EVar x)
tiExpr _ (_ :< ELit l) = tiLit l >>= \t -> return ([], t :< ELit l)
tiExpr env (_ :< EAp e1 e2) = do
    (ps, te1 :< e1') <- tiExpr env e1
    (qs, te2 :< e2') <- tiExpr env e2
    t <- newTVar KStar
    unify (makeFun te2 t) te1
    return (ps ++ qs, t :< EAp (te1 :< e1') (te2 :< e2'))
tiExpr (TypeEnv env) (_ :< ELet bg e) = do
    (ps, (TypeEnv env', bg')) <- tiBindingGroup (TypeEnv env) bg
    (qs, t :< e') <- tiExpr (TypeEnv (Map.union env' env)) e
    return (ps ++ qs, t :< ELet bg' (t :< e'))

-- | Run type inference for the expression in a binding and unify it with t.
-- Because t takes on the type of the bound identifier, env should map b's
-- identifier to t to allow for recursive bindings.
tiBoundExpr :: TypeEnv
            -> Binding a SyntacticExpr
            -> Type
            -> TI ([Pred], TypedExpr)
tiBoundExpr (TypeEnv env) b t = do
    -- Create a fresh type variable for each argument.
    targs <- mapM (const (newTVar KStar)) (arguments b)
    let schemes = map toScheme targs
        env' = Map.union (Map.fromList (zip (arguments b) schemes)) env
    (ps, te :< e) <- tiExpr (TypeEnv env') (body b)
    unify t $ foldr makeFun te targs
    return (ps, te :< e)

equivalentTypes :: Type -> Type -> State Subst Bool
equivalentTypes (TVar tyv1) (TVar tyv2) = do
    s <- get
    case Map.lookup tyv1 s of
        Just t -> return $ t == TVar tyv2
        Nothing -> do
            put $ Map.insert tyv1 (TVar tyv2) s
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
    s <- get
    case Map.lookup tyv1 s of
        Just (TVar tyv2') -> return $ tyv2 == tyv2'
        Just _ -> return False
        Nothing -> do
            put $ Map.insert tyv1 (TVar tyv2) s
            return True

-- | Returns True if there is a substitution mapping the first argument to the
-- second.
equivalentSchemes :: Scheme -> Scheme -> State Subst Bool
equivalentSchemes (Scheme tyvs1 qt1) (Scheme tyvs2 qt2) = do
    b1s <- zipWithM equivalentTyvars tyvs1 tyvs2
    b2 <- equivalentQuals qt1 qt2
    return $ and b1s && b2

-- TODO Split predicates into deferred and retained predicates.
tiExplBinding :: Infer (Binding Scheme SyntacticExpr) TypedBinding
tiExplBinding (TypeEnv env) b = do
    let schemeAnnot = annot b
    Qual qs t <- inst schemeAnnot
    (ps, e) <- tiBoundExpr (TypeEnv env) b t
    s <- getSubst
    let qs' = apply s qs
        t' = apply s t
        e' = apply s e
        schemeInferred = gen (apply s (TypeEnv env)) (Qual qs' t')
    let st = equivalentSchemes schemeInferred schemeAnnot
        equivalent = evalState st empty
    if not equivalent then throwError . InferenceError $ "signature too general"
    else return (apply s ps, b { body = e' })

-- | Run type inference for each binding in the binding group. Every expression
-- within the group can see the instantiated type variables representing every
-- other.
-- TODO Split predicates into deferred and retained predicates.
-- TODO Implement the monomorphism restriction.
tiImplBindingGroup ::
    Infer (BindingGroup () SyntacticExpr) (TypeEnv, TypedBindingGroup)
tiImplBindingGroup (TypeEnv env) bg = do
    -- Allocate a type variable for each binding.
    ts <- mapM (const (newTVar KStar)) bg
    let identifiers = map identifier bg
        schemes = map toScheme ts
        -- Create an environment mapping each identifier to its type scheme.
        env' = Map.union (Map.fromList (zip identifiers schemes)) env
    -- Unify the type allocated for each binding with the type inferred for its
    -- expression.
    bindingsResult <- zipWithM (tiBoundExpr (TypeEnv env')) bg ts
    let (pss, es) = (map fst bindingsResult, map snd bindingsResult)
    s <- getSubst
    let ps' = apply s (concat pss)
        ts' = apply s ts
        es' = apply s es
        -- Generalize all the types we found for the newly-bound names.
        schemes' = map (gen (apply s (TypeEnv env)) . Qual ps') ts'
    let bg' = zipWith3 (\b sc e -> b { annot = sc, body = e }) bg schemes' es'
    return (ps', (TypeEnv (Map.fromList (zip identifiers schemes')), bg'))

tiBindingGroup :: Infer (BindingGroup (Maybe Scheme) SyntacticExpr) (TypeEnv,
        TypedBindingGroup)
tiBindingGroup (TypeEnv env) bg = do
    let (expls, impls) = partitionByAnnotation bg
        TypeEnv explEnv = envFromExplBindingGroup expls
        env' = Map.union explEnv env
    (ps, (TypeEnv env'', typedImpls)) <- tiImplBindingGroup (TypeEnv env') impls
    let fullEnv = TypeEnv $ env'' `Map.union` env'
    bindingResults <- mapM (tiExplBinding fullEnv) expls
    let (qss, typedExpls) = (map fst bindingResults, map snd bindingResults)
    return (ps ++ concat qss, (fullEnv, typedExpls ++ typedImpls))

envFromExplBindingGroup :: BindingGroup Scheme SyntacticExpr -> TypeEnv
envFromExplBindingGroup bg = TypeEnv $ foldr addAnnot Map.empty annotPairs
  where
    addAnnot (v, schema) = Map.insert v schema
    annotPairs =
        [(v, schema) | Binding { identifier = v, annot = schema } <- bg]

partitionByAnnotation :: BindingGroup (Maybe Scheme) SyntacticExpr
                      -> ( BindingGroup Scheme SyntacticExpr
                         , BindingGroup () SyntacticExpr
                         )
partitionByAnnotation = partitionEithers . map f
  where
    f x = case annot x of Just sc -> Left (x { annot = sc })
                          Nothing -> Right (x { annot = () })

-- | Enhance a type inferencer to run over a list rather than a single entity,
-- accumulating an environment as it goes.
tiSequence :: Infer a (TypeEnv, TypedBindingGroup)
           -> Infer [a] (TypeEnv, TypedProgram)
tiSequence _ env [] = return ([], (env, []))
tiSequence ti (TypeEnv env) (x : xs) = do
    (ps, (TypeEnv tmpEnv, bg)) <- ti (TypeEnv env) x
    let env' = Map.union tmpEnv env
    (qs, (TypeEnv env'', bg')) <- tiSequence ti (TypeEnv env') xs
    return (ps ++ qs, (TypeEnv (Map.union env'' env'), bg : bg'))

-- TODO Deal with ambiguities and defaults.
tiProgram :: TypeEnv -> Program (Maybe Scheme) SyntacticExpr ->
    Either InferenceError ([Pred], TypeEnv, TypedProgram)
tiProgram env bgs = runTI $ do
    (ps, (env', bgs')) <- tiSequence tiBindingGroup env bgs
    s <- getSubst
    return (ps, apply s env', apply s bgs')

