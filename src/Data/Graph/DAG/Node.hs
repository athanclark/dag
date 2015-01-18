module Data.Graph.DAG.Node where

-- | Data type for representing the nodes of a graph. We don't allow arbitrary
-- addition of empty nodes, yet, because there is a bound enforced by edges
-- already drawn.
data NodeSchema = Conservative -- ^ Nodes are available as edges are drawn
                | Exhaustive   -- ^ All possible nodes exist
