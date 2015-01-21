{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Graph.DAG
        ( module Data.Graph.DAG.Edge
        , module Data.Graph.DAG.Edge.Utils
        , DAG (..)
        , glookup
        ) where

import Data.Graph.DAG.Edge
import Data.Graph.DAG.Edge.Utils

import Data.List (lookup)
import Data.Singletons
import Data.Proxy

-- | The graph may be not connected
data DAG es a = forall x unique. GNil (EdgeSchema es x unique)
              | GCons String a (DAG es a)

instance Functor (DAG es) where
  fmap f (GNil e) = GNil e
  fmap f (GCons k x xs) = GCons k (f x) $
    fmap f xs

-- | Convenience function.
-- getEdgeSchema :: DAG es
getEdgeSchema (GNil e) = e
getEdgeSchema (GCons _ _ gs) = getEdgeSchema gs

-- | A simple @Data.Map.lookup@ duplicate.
glookup :: String -> DAG es a -> Maybe a
glookup _ (GNil _) = Nothing
glookup k (GCons k2 a gs) | k == k2   = Just a
                          | otherwise = glookup k gs

-- | Get the spanning trees of an @EdgeSchema@. Operate on the assumtion that
-- the data returned is actually @[Tree String]@.
espanningtrees :: SingI (SpanningTrees' es '[]) =>
                  EdgeSchema es x unique
               -> Demote (SpanningTrees' es '[])
espanningtrees = reflect . getSpanningTrees

-- gspanningtrees


{-
gtree :: String -> DAG es a -> Maybe (Tree a)
gtree k g = lookup k $ force $ reflect $ getSpanningTrees $ getEdgeSchema g
-}
