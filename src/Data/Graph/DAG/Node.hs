module Data.Graph.DAG.Node where

import Data.Monoid

-- | This is just a simple inductive list
data NodeSchema a = GNil
                  | GCons String a (NodeSchema a)
  deriving (Show, Eq)

instance Functor NodeSchema where
  fmap f GNil = GNil
  fmap f (GCons k x xs) = GCons k (f x) $ fmap f xs

-- | Simple lookup function.
nlookup :: String -> NodeSchema a -> Maybe a
nlookup _ GNil = Nothing
nlookup k1 (GCons k2 x xs) | k1 == k2 = Just x
                           | otherwise = nlookup k1 xs

-- | We overwrite with rightward prescedence.
ncombine :: NodeSchema a -> NodeSchema a -> NodeSchema a
ncombine GNil ys = ys
ncombine (GCons k1 x xs)
         (GCons k2 y ys) | k1 == k2 =
  GCons k1 y $ ncombine xs ys
                         | otherwise =
  GCons k1 x $ GCons k2 y $ ncombine xs ys

-- | Delete a node from a collection of nodes.
nremove :: String -> NodeSchema a -> NodeSchema a
nremove _ GNil = GNil
nremove k1 (GCons k2 x xs) | k1 == k2 = xs
                           | otherwise = nremove k1 xs

-- | Overwrite a node to a collection of nodes. In theory, because we don't
-- export @GCons@, uniqueness is garunteed through the constructive use of this
-- function, therefore recursing down after an overwrite is unnecessary.
nadd :: String -> a -> NodeSchema a -> NodeSchema a
nadd _ _ GNil = GNil
nadd k1 a (GCons k2 x xs) | k1 == k2 = GCons k1 a xs

instance Monoid (NodeSchema a) where
  mempty = GNil
  mappend = ncombine
