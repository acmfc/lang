module Lang.PrettyPrint (printTypeEnv, prettyPrint) where

import Text.PrettyPrint
import Data.List (inits)
import qualified Data.Map as Map
import Data.Maybe (isJust)

import Lang.Type

class PrettyPrint p where
    prettyPrint :: p -> Doc

extractFunctionType :: Type -> Maybe (Type, Type)
extractFunctionType (TAp (TAp tc t1) t2) | tc == tArrow = Just (t1, t2)
extractFunctionType _ = Nothing

isFunction :: Type -> Bool
isFunction = isJust . extractFunctionType

conditionalParens :: Bool -> Doc -> Doc
conditionalParens True = parens
conditionalParens False = id

extractRecordType :: Type -> Maybe Type
extractRecordType (TAp tc t) | tc == tRecordCon = Just t
extractRecordType _ = Nothing

instance PrettyPrint Type where
    prettyPrint (TVar (Tyvar tvn _)) = text tvn
    prettyPrint (TCon (Tycon s _)) = text s
    prettyPrint t@(TAp t1 t2) = case extractFunctionType t of
        Just (t1', t2') -> ppt1' <+> text "->" <+> prettyPrint t2'
          where
            ppt1' = conditionalParens (isFunction t1') (prettyPrint t1')
        Nothing -> case extractRecordType t of
            Just t' -> prettyPrint t'
            Nothing -> prettyPrint t1 <+> prettyPrint t2

-- TODO Rename row variables.
instance PrettyPrint Row where
    prettyPrint (RVar t) = prettyPrint t
    prettyPrint REmpty = text "{}"
    prettyPrint (RExt t1 t2 r) =
        ppExt (prettyPrint t1) (prettyPrint t2) (prettyPrint r)
          where
            ppExt ppt1 ppt2 ppr =
                braces $ ppt1 <+> text ":" <+> ppt2 <+> text "|" <+> ppr

instance PrettyPrint Pred where
    prettyPrint (RowLacks t1 t2) = parens $ ppt1 <> text "\\" <> ppt2
      where
        ppt1 = prettyPrint t1
        ppt2 = prettyPrint t2
    prettyPrint (RowEq t1 t2) = prettyPrint t1 <> text "~" <> prettyPrint t2

instance PrettyPrint t => PrettyPrint (Qual t) where
    prettyPrint (Qual ps t) = withPrefix $ prettyPrint t
      where
        withPrefix x =
            if not (null predsToPrint) then ppPreds <+> text "=>" <+> x else x
        predsToPrint = filter (not . isLacks) ps
        isLacks p = case p of (RowLacks _ _) -> True; _ -> False
        ppPreds = hsep (map prettyPrint predsToPrint)

instance PrettyPrint Scheme where
    prettyPrint (Scheme [] qt) = prettyPrint qt
    prettyPrint (Scheme tvs qt) = quantifiers <> text "." <+> prettyPrint qt'
      where
        quantifiers =
            text "forall" <+> (hcat . punctuate space) (map text tvns')
        tvns' = map (\(Tyvar tvn _) -> tvn) tvs'
        Scheme tvs' qt' = renameSchemeVariables (Scheme tvs qt)

primedNames :: String -> [TypeVariableName]
primedNames alphabet = concatMap (\suffix -> map (: suffix) alphabet) suffixes
  where
    suffixes = inits $ cycle "'"

indexedNames :: Char -> [TypeVariableName]
indexedNames c = [c] : map ((:) c . show) ([1..] :: [Integer])

renameSchemeVariables :: Scheme -> Scheme
renameSchemeVariables (Scheme tvs qt) = Scheme renamedTvs qt'
  where
    qt' = apply sub qt
    sub = sub1 `compose` sub2 `compose` sub3
    sub1 = Map.fromList $ zip kstarTvs $ map TVar renamedKstarTvs
    sub2 = Map.fromList $ zip klabTvs $ map TVar renamedKlabTvs
    sub3 = Map.fromList $ zip krowTvs $ map TVar renamedKrowTvs
    renamedTvs = renamedKstarTvs ++ renamedKlabTvs ++ renamedKrowTvs
    renamedKstarTvs = renameTvs (primedNames "abcdefgh") kstarTvs
    renamedKlabTvs = renameTvs (primedNames "lmno") klabTvs
    renamedKrowTvs = renameTvs (indexedNames 'r') krowTvs
    kstarTvs = filter (hasKind KStar) tvs
    klabTvs = filter (hasKind KLab) tvs
    krowTvs = filter (hasKind KRow) tvs
    hasKind k (Tyvar _ k') = k == k'

renameTvs :: [TypeVariableName] -> [Tyvar] -> [Tyvar]
renameTvs newNames tvs =
    map (\(name, Tyvar _ k) -> Tyvar name k) $ zip newNames tvs

printTypeEnv :: TypeEnv -> [String]
printTypeEnv (TypeEnv env) = map (\(name, scheme) ->
    name ++ " : " ++ (render . prettyPrint) scheme) (Map.toList env)

