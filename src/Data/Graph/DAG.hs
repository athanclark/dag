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
import Data.Maybe (fromJust)

-- | A (potentially sparse) directed acyclic graph, composed of edges and nodes.
data DAG es x u a = DAG { getEdgeSchema :: (EdgeSchema es x u)
                        , getNodeSchema :: (NodeSchema a)
                        }

instance Functor (DAG es x u) where
  fmap f (DAG es xs) = DAG es $ fmap f xs

-- | @Data.Map.lookup@ duplicate.
glookup :: String -> DAG es x u a -> Maybe a
glookup k (DAG _ xs) = nlookup k xs

-- | Spanning trees of a graph.
gspanningtrees :: SingI (SpanningTrees' es '[]) =>
                  DAG es x u a -> [RTree a]
gspanningtrees g = fmap replace $ espanningtrees $ getEdgeSchema g
  where
  replace = fmap $ fromJust . flip glookup g

-- | Spanning tree of a particular node. "A possible tree of possible results"
gtree :: SingI (SpanningTrees' es '[]) =>
         String -> DAG es x unique a -> Maybe (RTree a)
gtree k g = fmap (fmap $ fromJust . flip glookup g) $ etree k $ getEdgeSchema g
