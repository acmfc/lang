{-|
Implements function-level dependency analysis.
-}

module Lang.DependencyAnalysis where

import Control.Comonad.Cofree
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lang.Identifier
import Lang.Core

-- | Find all VariableNames referenced in an Expr.
exprDependencies :: Expr a b
                 -> Set.Set VariableName
                 -> Set.Set VariableName
exprDependencies (_ :< EVar x) ignore | Set.member x ignore = Set.empty
                                      | otherwise = Set.singleton x
exprDependencies (_ :< ELit _) _ = Set.empty
exprDependencies (_ :< EAp e1 e2) ignore =
    Set.union (exprDependencies e1 ignore) (exprDependencies e2 ignore)
exprDependencies (_ :< ELet bg e) ignore = Set.union bgDeps eDeps
  where
    bgDeps = Set.unions $ map (`bindingDependencies` boundNames) bg
    eDeps = exprDependencies e (Set.union ignore boundNames)
    boundNames = Set.fromList (map identifier bg)

-- | Find all VariableNames referenced in a Binding.
bindingDependencies :: Binding a (Expr a b) -> Set.Set VariableName -> Set.Set VariableName
bindingDependencies b ignore = exprDependencies (body b) ignore'
  where
    ignore' = Set.union (Set.fromList (identifier b : arguments b)) ignore

-- Two sorts of dependency graphs are used below:
--
-- * Binding dependency graph: a node is a Binding, and an edge from i to j
-- with label x indicates that i depends on a symbol x which is defined by j.
--
-- * BindingGroup dependency graph: a node is a BindingGroup and an edge from i
-- to j indicates that some Binding in i depends on some symbol defined in j.
--
-- We use Data.Graph to run strongly connected components and topological sort
-- algorithms over our graphs. It requires that every node have a unique key.
-- In a Binding dependency graph, a key is a VariableName equal to the symbol
-- defined by the Binding. In a BindingGroup dependency graph, a key is a
-- BindingGroupId - a unique value arbitrarily assigned to the BindingGroup.

-- | Represents a strongly connected component in the Binding dependency graph.
-- Contains a list of nodes where each node contains a Binding, a key that
-- uniquely identifies that Binding, and a list of dependencies.
type Scc a b = [(Binding a b, VariableName, [VariableName])]

-- | Build a dependency graph from a list of bindings and return a list of
-- strongly connected components, so that each component represents a minimal
-- binding group.
minimalBindingGroups :: [Binding a (Expr a b)] -> [Scc a (Expr a b)]
minimalBindingGroups bs = map Graph.flattenSCC sccs
  where
    sccs = Graph.stronglyConnCompR adjacencyList
    adjacencyList = map (\b -> (b, identifier b, listDependencies b)) bs
    listDependencies b = Set.toList (bindingDependencies b Set.empty)

type BindingGroupId = Int

-- | Describes an intermediate stage between a Binding dependency graph and a
-- BindingGroup dependency graph. LabeledSccs are constructed from the former
-- and used to construct the latter.
type LabeledScc a b = (Scc a b, BindingGroupId)

-- | Assign an arbitrary BindingGroupId to each Scc.
labelSccs :: [Scc a (Expr a b)] -> [LabeledScc a (Expr a b)]
labelSccs sccs = zip sccs [0..]

-- | Maps a VariableName to the BindingGroupId for the BindingGroup in which it
-- was defined.
type BindingGroupIdMap = Map.Map VariableName BindingGroupId

-- | Collect all names bound in an Scc and map them to the BindingGroupId
-- allocated to that Scc.
makeBindingGroupIdMap :: LabeledScc a (Expr a b) -> BindingGroupIdMap
makeBindingGroupIdMap (scc, n) = Map.fromList assocList
  where
    assocList = zip (boundNames scc) (repeat n)
    boundNames = map identifier . getBindings
    getBindings = map (\(b, _,  _) -> b)

-- | Collect BindingGroup dependencies for every Binding dependency that's
-- present in bindingGroupIdMap.
lookupBindingGroupDependencies ::
    [VariableName] -> BindingGroupIdMap -> [BindingGroupId]
lookupBindingGroupDependencies vns bindingGroupIdMap =
    foldr (\vn bgids -> case Map.lookup vn bindingGroupIdMap of
            Just bgid -> bgid : bgids
            Nothing -> bgids) [] vns

type BindingGroupDepNode a b =
    (BindingGroup a b, BindingGroupId, [BindingGroupId])

-- | Convert a node in the Binding dependency graph to one in the BindingGroup
-- dependency graph.
makeBindingGroupDepNode :: BindingGroupIdMap
                        -> LabeledScc a (Expr a b)
                        -> BindingGroupDepNode a (Expr a b)
makeBindingGroupDepNode bindingGroupIdMap (scc, n) =
    (bg, n, bindingGroupDependencies)
      where
        (bg, _, vnss) = unzip3 scc
        bindingGroupDependencies =
            lookupBindingGroupDependencies (concat vnss) bindingGroupIdMap

-- | Construct a BindingGroup dependency graph from a Binding dependency graph.
makeBindingGroupDepGraph :: [Scc a (Expr a b)] ->
    ( Graph.Graph
    , Graph.Vertex -> BindingGroupDepNode a (Expr a b)
    , BindingGroupId -> Maybe Graph.Vertex
    )
makeBindingGroupDepGraph sccs = Graph.graphFromEdges nodes
  where
    nodes = map (makeBindingGroupDepNode bindingGroupIdMap) labeledSccs
    bindingGroupIdMap = Map.unions $ map makeBindingGroupIdMap labeledSccs
    labeledSccs = labelSccs sccs

-- | Run a topological sort on a BindingGroup dependency graph and extract the
-- resulting BindingGroups, ordered so that each element in the list depends
-- only on those before it.
orderBindingGroups :: ( Graph.Graph
                      , Graph.Vertex -> BindingGroupDepNode a (Expr a b)
                      , BindingGroupId -> Maybe Graph.Vertex
                      ) -> [BindingGroup a (Expr a b)]
orderBindingGroups (graph, vertexToNode, _) =
    map (extractBindingGroups . vertexToNode) orderedVertices
      where
        extractBindingGroups node = let (bg, _, _) = node in bg
        orderedVertices = reverse $ Graph.topSort graph

-- | Arrange a list of Bindings into a list of BindingGroups so that each
-- BindingGroup contains a minimal set of mutually recursive Bindings and
-- depends only on those that come before it in the resulting list.
structureBindings :: [Binding a (Expr a b)] -> [BindingGroup a (Expr a b)]
structureBindings =
    orderBindingGroups . makeBindingGroupDepGraph . minimalBindingGroups

