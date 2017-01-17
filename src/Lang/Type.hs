module Lang.Type where

import qualified Data.Map as Map

type TypeVariableName = String
data Type = TVar TypeVariableName
  | TCons String [Type]
  deriving (Show, Eq)

arrow :: Type -> Type -> Type
arrow t1 t2 = TCons "arrow" [t1, t2]

int :: Type
int = TCons "int" []

typeVariables :: Type -> [TypeVariableName]
typeVariables t = typeVariables' t []
  where
    typeVariables' (TVar tvn) accum = tvn:accum
    typeVariables' (TCons _ ts) accum = foldr typeVariables' accum ts

type Subst = Map.Map TypeVariableName Type

apply :: Subst -> Type -> Type
apply sub (TVar tvn) = Map.findWithDefault (TVar tvn) tvn sub
apply sub (TCons tcon ts) = TCons tcon (map (apply sub) ts)

compose :: Subst -> Subst -> Subst
compose sub1 sub2 = Map.map (apply sub1) sub2 `Map.union` sub1

singleton :: TypeVariableName -> Type -> Subst
singleton = Map.singleton
