{-|
This module implements function-level dependency analysis.
-}

module Lang.DependencyAnalysis ( exprDependencies
                               , bindingDependencies
                               , structureBindings ) where

import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lang.Core

-- | Find all VariableNames referenced in an Expr.
exprDependencies :: Expr -> Set.Set VariableName -> Set.Set VariableName
exprDependencies (EVar x) ignore | Set.member x ignore = Set.empty
                                 | otherwise = Set.singleton x
exprDependencies (ELit _) _ = Set.empty
exprDependencies (EAp e1 e2) ignore =
    Set.union (exprDependencies e1 ignore) (exprDependencies e2 ignore)
exprDependencies (ELet bg e) ignore = Set.union bgDeps eDeps
  where
    bgDeps = Set.unions $ map (flip bindingDependencies boundNames) bg
    eDeps = exprDependencies e (Set.union ignore boundNames)
    boundNames = Set.fromList (map identifier bg)

-- | Find all VariableNames referenced in a Binding.
bindingDependencies :: Binding -> Set.Set VariableName -> Set.Set VariableName
bindingDependencies b ignore = exprDependencies (body b) ignore'
  where
    ignore' = Set.union (Set.fromList (identifier b : arguments b)) ignore

-- Dependency graphs are represented below in two ways:
-- * In the first, a node is a Binding, and an edge from i to j with label x
-- indicates that i depends on a symbol x which is defined by j.
-- * In the second, a node is a BindingGroup and an edge from i to j indicates
-- that some Binding in i depends on some symbol defined in j.
--
-- We use Data.Graph to run strongly connected components and topological sort
-- algorithms over our graphs. It requires that every node have an alternative,
-- unique key. In the first type of graph, a key is a VariableName equal to the
-- symbol defined by the Binding. In the second type of graph, a key is a
-- BindingGroupId - a unique value arbitrarily assigned to the BindingGroup.

-- | Represents a strongly connected component in the Binding dependency graph.
-- Contains a list of nodes where each node contains a Binding, a key that
-- uniquely identifies that Binding, and a list of dependencies.
type Scc = [(Binding, VariableName, [VariableName])]

-- | Build a dependency graph from a list of bindings and return a list of
-- strongly connected components, where each component represents a minimal
-- binding group.
minimalBindingGroups :: [Binding] -> [Scc]
minimalBindingGroups bs = map Graph.flattenSCC sccs
  where
    sccs = Graph.stronglyConnCompR adjacencyList
    adjacencyList = map (\b -> (b, identifier b, listDependencies b)) bs
    listDependencies b = Set.toList (bindingDependencies b Set.empty)

type LabeledScc = (Scc, BindingGroupId)

-- Need to map VariableName -> Scc ID (Int)
-- Associate an Int with each Scc:
-- zip [0..] (minimalBindingGroups bindings)
labelSccs :: [Scc] -> [LabeledScc]
labelSccs sccs = zip sccs [0..]

type BindingGroupId = Int
type BindingGroupIdMap = Map.Map VariableName BindingGroupId

-- | Collect all names bound in scc and map them to n.
makeBindingGroupIdMap :: LabeledScc -> BindingGroupIdMap
makeBindingGroupIdMap (scc, n) = Map.fromList assocList
  where
    assocList = zip (boundNames scc) (repeat n)
    boundNames = map identifier . getBindings
    getBindings = map (\(b, _,  _) -> b)

lookupBindingGroupIds :: [VariableName] -> BindingGroupIdMap -> [Int]
lookupBindingGroupIds vns bindingGroupIdMap= foldr (\vn bids -> case (Map.lookup vn bindingGroupIdMap) of
    Just bid -> bid : bids
    Nothing -> bids) [] vns

type DagNode = (BindingGroup, BindingGroupId, [BindingGroupId])

makeDagNode :: BindingGroupIdMap -> LabeledScc -> DagNode
makeDagNode bindingGroupIdMap (scc, n) = let (bg, _, vnss) = unzip3 scc in
    (bg, n, lookupBindingGroupIds (concat vnss) bindingGroupIdMap)

dependencyDag :: [Scc] ->
    (Graph.Graph, Graph.Vertex -> DagNode, BindingGroupId -> Maybe Graph.Vertex)
dependencyDag sccs = Graph.graphFromEdges nodes
  where
    nodes = map (makeDagNode bindingGroupIdMap) labeledSccs
    bindingGroupIdMap = Map.unions $ map makeBindingGroupIdMap labeledSccs
    labeledSccs = labelSccs sccs

orderBindingGroups :: [Scc] -> [BindingGroup]
orderBindingGroups sccs =
    map (extractBindingGroups . vertexToNode) orderedVertices
      where
        extractBindingGroups node = let (bg, _, _) = node in bg
        (graph, vertexToNode, _) = dependencyDag sccs
        orderedVertices = reverse $ Graph.topSort graph

structureBindings :: [Binding] -> [BindingGroup]
structureBindings bs = orderBindingGroups $ minimalBindingGroups bs
