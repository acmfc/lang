module Lang.Type where

type TypeVariableName = String
data Type = TVar TypeVariableName
  | TCons String [Type]

arrow :: Type -> Type -> Type
arrow t1 t2 = TCons "arrow" [t1, t2]

int :: Type
int = TCons "int" []

typeVariables :: Type -> [TypeVariableName]
typeVariables t = typeVariables' t []
  where
    typeVariables' (TVar x) accum = x:accum
    typeVariables' (TCons x ts) accum = foldr typeVariables' accum ts

