module Lang.PrettyPrint where

import Text.PrettyPrint
import qualified Data.Map as Map

import Lang.Type

class PrettyPrint p where
    prettyPrint :: p -> Doc

instance PrettyPrint Type where
    prettyPrint (TVar tvn) = text tvn
    prettyPrint TInt = text "Int"
    prettyPrint (TFun (TFun t1 t2) t3) =
        parens (prettyPrint (TFun t1 t2)) <+> text "->" <+> prettyPrint t3
    prettyPrint (TFun t1 t2) = prettyPrint t1 <+> text "->" <+> prettyPrint t2

instance PrettyPrint Scheme where
    prettyPrint (Scheme [] t) = prettyPrint t
    prettyPrint (Scheme tvns t) = quantifiers <> text "." <+> prettyPrint t'
      where
        quantifiers =
            text "forall" <+> (hcat . punctuate space) (map text tvns')
        Scheme tvns' t' = renameSchemeVariables (Scheme tvns t)

renameSchemeVariables :: Scheme -> Scheme
renameSchemeVariables (Scheme tvns t) = Scheme tvns' t'
  where
    t' = apply sub t
    tvns' = map (\tvn -> case (lookup tvn nameSub) of
            Just tvn' -> tvn'
            Nothing -> tvn) tvns
    sub = Map.fromList $ map (\(tvn, s) -> (tvn, TVar s)) nameSub
    nameSub = zip tvns (map (: []) alphabet)
    alphabet = "abcdefghijklmnopqrstuvwxyz"

printTypeEnv :: TypeEnv -> [String]
printTypeEnv (TypeEnv env) = map (\(name, scheme) ->
    name ++ " : " ++ (render . prettyPrint) scheme) (Map.toList env)
