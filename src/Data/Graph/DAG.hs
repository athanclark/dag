{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Data.Graph.DAG
        ( module Data.Graph.DAG.Edge
        , DAG (..)
        , glookup
        ) where

import Data.Graph.DAG.Edge

data DAG es a where
  GNil :: EdgeSchema es x unique -> DAG es a
  GCons :: (String ~ key) =>
           key
        -> a -- value
        -> DAG es a
        -> DAG es a

instance Functor (DAG es) where
  fmap f (GNil e) = GNil e
  fmap f (GCons k x xs) = GCons k (f x) $
    fmap f xs


glookup :: String -> DAG es a -> Maybe a
glookup _ (GNil _) = Nothing
glookup k (GCons k2 a gs) | k == k2   = Just a
                          | otherwise = glookup k gs
