module Lang.PrettyPrint where

import Text.PrettyPrint
import Data.List (inits)
import qualified Data.Map as Map
import Data.Maybe (isJust)

import Lang.Type

class PrettyPrint p where
    prettyPrint :: p -> Doc

extractFunctionType :: Type -> Maybe (Type, Type)
extractFunctionType (TAp (TAp tycon t1) t2) | tycon == tArrow = Just (t1, t2)
extractFunctionType _ = Nothing

isFunction :: Type -> Bool
isFunction = isJust . extractFunctionType

conditionalParens :: Bool -> Doc -> Doc
conditionalParens True d = parens d
conditionalParens False d = d

instance PrettyPrint Type where
    prettyPrint (TVar (Tyvar tvn _)) = text tvn
    prettyPrint (TCon (Tycon s _)) = text s
    prettyPrint t@(TAp t1 t2) = case extractFunctionType t of
        Just (t1', t2') -> ppt1' <+> text "->" <+> prettyPrint t2'
          where
            ppt1' = conditionalParens (isFunction t1') (prettyPrint t1')
        Nothing -> text "TAp" <+> prettyPrint t1 <+> prettyPrint t2

instance PrettyPrint Pred where
    prettyPrint (Pred t) = prettyPrint t

instance PrettyPrint t => PrettyPrint (Qual t) where
    prettyPrint (Qual ps t) = withPrefix $ prettyPrint t
      where
        withPrefix x = if not (null ps) then ppPreds <+> text "=>" <+> x else x
        ppPreds = hsep (map prettyPrint ps)

instance PrettyPrint Scheme where
    prettyPrint (Scheme [] qt) = prettyPrint qt
    prettyPrint (Scheme tvs qt) = quantifiers <> text "." <+> prettyPrint qt'
      where
        quantifiers =
            text "forall" <+> (hcat . punctuate space) (map text tvns')
        tvns' = map (\(Tyvar tvn _) -> tvn) tvs'
        Scheme tvs' qt' = renameSchemeVariables (Scheme tvs qt)

freshTypeVariableNames :: [TypeVariableName]
freshTypeVariableNames = concatMap (\suffix -> map (: suffix) alphabet) suffixes
  where
    suffixes = inits $ cycle "'"
    alphabet = "abcdefghijklmnopqrstuvwxyz"

renameSchemeVariables :: Scheme -> Scheme
renameSchemeVariables (Scheme tvs qt) = Scheme renamedTvs qt'
  where
    qt' = apply sub qt
    sub = Map.fromList $ zip tvs $ map TVar renamedTvs
    renamedTvs = map (\(tvn, k) -> Tyvar tvn k) (zip freshTypeVariableNames ks)
    ks = map (\(Tyvar _ k) -> k) tvs

printTypeEnv :: TypeEnv -> [String]
printTypeEnv (TypeEnv env) = map (\(name, scheme) ->
    name ++ " : " ++ (render . prettyPrint) scheme) (Map.toList env)

