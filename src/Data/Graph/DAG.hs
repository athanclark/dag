{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.DAG
        ( module Data.Graph.DAG.Edge
        , module Data.Graph.DAG.Edge.Utils
        , DAG (..)
        , glookup
        ) where

import Data.Graph.DAG.Edge
import Data.Graph.DAG.Edge.Utils

import Data.List (lookup)
import Control.DeepSeq (force)

-- | The graph may be not connected, with dangling nodes, if desired.
data DAG es a where
  GNil :: forall es a x unique. EdgeSchema es x unique
       -> DAG es a
  GCons :: String
        -> a -- value
        -> DAG es a
        -> DAG es a
{-
-- | Convenience function.
getEdgeSchema :: DAG es a -> EdgeSchema es x unique
getEdgeSchema (GNil e) = (e :: EdgeSchema es x unique)
getEdgeSchema (GCons _ _ gs) = getEdgeSchema gs
-}
instance Functor (DAG es) where
  fmap f (GNil e) = GNil e
  fmap f (GCons k x xs) = GCons k (f x) $
    fmap f xs

-- | A simple @Data.Map.lookup@ duplicate.
glookup :: String -> DAG es a -> Maybe a
glookup _ (GNil _) = Nothing
glookup k (GCons k2 a gs) | k == k2   = Just a
                          | otherwise = glookup k gs
{-
gtree :: String -> DAG es a -> Maybe (Tree a)
gtree k g = lookup k $ force $ reflect $ getSpanningTrees $ getEdgeSchema g
-}
