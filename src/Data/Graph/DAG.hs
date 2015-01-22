{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Graph.DAG
        ( module Data.Graph.DAG.Edge
        , module Data.Graph.DAG.Edge.Utils
        , module Data.Graph.DAG.Node
        , DAG (..)
        , glookup
        ) where

import Data.Graph.DAG.Edge
import Data.Graph.DAG.Edge.Utils
import Data.Graph.DAG.Node

import Data.List (lookup)
import Data.Singletons
import Data.Proxy

-- | A (potentially sparse) directed acyclic graph, composed of edges and nodes.
data DAG es x u a = DAG (EdgeSchema es x u)
                        (NodeSchema a)

instance Functor (DAG es x u) where
  fmap f (DAG es xs) = DAG es $ fmap f xs

-- | Convenience function for extracting the edges of a graph.
getEdgeSchema :: DAG es x u a -> EdgeSchema es x u
getEdgeSchema (DAG es _) = es

-- | Likewise for nodes (just a simple map).
getNodeSchema :: DAG es x u a -> NodeSchema a
getNodeSchema (DAG _ xs) = xs

-- | @Data.Map.lookup@ duplicate.
glookup :: String -> DAG es x u a -> Maybe a
glookup k (DAG _ xs) = nlookup k xs

-- | Spanning trees of a graph.
gspanningtrees :: SingI (SpanningTrees' es '[]) =>
                  DAG es x u a -> [Tree (Maybe a)]
gspanningtrees g = fmap replace $ espanningtrees $ getEdgeSchema g
  where
  replace = fmap $ flip glookup g

-- | Spanning tree of a particular node. "A possible tree of possible results"
gtree :: SingI (SpanningTrees' es '[]) =>
         String -> DAG es x unique a -> Maybe (Tree (Maybe a))
gtree k g = fmap (fmap $ flip glookup g) $ etree k $ getEdgeSchema g
