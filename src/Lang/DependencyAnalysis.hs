module Lang.DependencyAnalysis where

import qualified Data.Graph as Graph
import qualified Data.Set as Set
import Lang.Core

-- | Find all VariableNames referenced in an Expr.
exprDependencies :: Expr -> Set.Set VariableName -> Set.Set VariableName
exprDependencies (EVar x) ignore =
    if Set.member x ignore then Set.empty else Set.singleton x
exprDependencies (ELit _) _ = Set.empty
exprDependencies (EAp e1 e2) ignore =
    Set.union (exprDependencies e1 ignore) (exprDependencies e2 ignore)
exprDependencies (ELet bg e) ignore = Set.union bgDeps eDeps
  where
    bgDeps = Set.unions $ map (flip bindingDependencies ignore) bg
    eDeps = exprDependencies e (Set.union ignore boundNames)
    boundNames = Set.fromList (map identifier bg)

-- | Find all VariableNames referenced in a Binding.
bindingDependencies :: Binding -> Set.Set VariableName -> Set.Set VariableName
bindingDependencies b ignore = exprDependencies (body b) ignore'
  where
    ignore' = Set.union (Set.fromList (arguments b)) ignore

-- | Group an unorganized list of bindings into minimal mutually recursive
-- binding groups.
minimalBindingGroups :: [Binding] -> [BindingGroup]
minimalBindingGroups bs = map Graph.flattenSCC scc
  where
    scc = Graph.stronglyConnComp adjacencyList
    adjacencyList = map (\b -> (b, identifier b, listDependencies b)) bs
    listDependencies b = Set.toList (bindingDependencies b Set.empty)

