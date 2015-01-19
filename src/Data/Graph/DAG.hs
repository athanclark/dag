{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Data.Graph.DAG where

import Data.Graph.DAG.Edge

data DAG es a where
  GNil :: EdgeSchema es x unique -> DAG es a
  GCons :: (String ~ key) =>
           key
        -> a -- value
        -> DAG es a
        -> DAG es a

glookup :: String -> DAG es a -> Maybe a
glookup _ (GNil _) = Nothing
glookup k (GCons k2 a gs) | k == k2   = Just a
                          | otherwise = glookup k gs
