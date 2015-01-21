{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.DAG.Edge.Utils where

import Data.Graph.DAG.Edge

import GHC.TypeLits
import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Proxy
import Data.Monoid
import Control.Applicative

-- | Trivial rose tree for creating spanning trees. We make control structure
-- instances "parallel" (instead of cartesian) by default for simplicity.
$(singletons [d|
  data Tree a = Node a [Tree a] deriving (Show, Eq, Functor)
  |])

instance Applicative Tree where
  pure a = Node a []
  (Node f fs) <*> (Node x xs) = Node (f x) $
    zipWith (<*>) fs xs

instance Monad Tree where
  return = pure
  (Node x xs) >>= f = case f x of -- Substitution based instance.
    (Node y ys) -> Node y $ fmap (>>= f) xs

instance Monoid a => Monoid (Tree a) where
  mempty = Node mempty []
  (Node x xs) `mappend` (Node y ys) = Node (x `mappend` y) $
    zipWith mappend xs ys

-- | Gives us a generic way to get our spanning trees of the graph, as a value.
-- Credit goes to <stackoverflow.com/questions/28030118/reflecting-heterogeneous-promoted-types-back-to-values-compositionally András Kovács>.
reflect ::
  forall (a :: k).
  (SingI a, SingKind ('KProxy :: KProxy k)) =>
  Proxy a -> Demote a
reflect _ = fromSing (sing :: Sing a)

-- | Adds an empty @c@ tree to the list of trees uniquely
type family AppendIfNotElemTrees (c :: k) (trees :: [Tree k]) :: [Tree k] where
  AppendIfNotElemTrees c ((Node c xs) ': xss) = (Node c xs) ': xss
  AppendIfNotElemTrees c ((Node x xs) ': xss) = (Node x xs) ':
    (AppendIfNotElemTrees c xss)
  AppendIfNotElemTrees c '[] = (Node c '[]) ': '[]

-- | Adds @c@ as a child of any tree with a root @t@. Assumes unique roots.
type family AddChildTo (test :: k)
                       (child :: k)
                       (trees :: [Tree k]) :: [Tree k] where
  AddChildTo t c ((Node t xs) ': xss) =
    (Node t (AppendIfNotElemTrees c xs)) ': (AddChildTo t c xss)
  AddChildTo t c ((Node x xs) ': xss) =
    (Node x (AddChildTo t c xs)) ': (AddChildTo t c xss)
  AddChildTo t c '[] = '[]

-- | We need to track if @from@ has is a root node or not. TODO: Some code repeat.
type family AddEdge' (edge :: EdgeKind)
                     (trees :: [Tree Symbol])
                     (hasFromRoot :: Bool)
                     (hasToRoot :: Bool):: [Tree Symbol] where
  AddEdge' ('EdgeType from to) '[] 'False 'False =
    (Node from ((Node to '[]) ': '[])) ': (Node to '[]) ': '[]

  AddEdge' ('EdgeType from to) '[] 'True 'False =
    (Node to                     '[])  ':                  '[]

  AddEdge' ('EdgeType from to) '[] 'False 'True =
    (Node from ((Node to '[]) ': '[])) ':                  '[]

  AddEdge' x '[] 'True 'True = '[]

  AddEdge' ('EdgeType from to) ((Node from xs) ': xss) hasFromRoot hasToRoot =
    (Node from (AppendIfNotElemTrees to xs)) ':
      (AddEdge' ('EdgeType from to) xss 'True hasToRoot)

  AddEdge' ('EdgeType from to) ((Node to xs) ': xss) hasFromRoot hasToRoot =
    (Node to (AddEdge' ('EdgeType from to) xs 'True 'True)) ':
      (AddEdge' ('EdgeType from to) xss hasFromRoot 'True)

  -- Go downward, and laterally (I think).
  AddEdge' ('EdgeType from to) ((Node x xs) ': xss) hasFromRoot hasToRoot =
    (Node x (AddEdge' ('EdgeType from to) xs 'True 'True)) ':
      (AddEdge' ('EdgeType from to) xss hasFromRoot hasToRoot)

-- | Add @to@ as a child to every @from@ node in the accumulator.
type family AddEdge (edge :: EdgeKind)
                    (trees :: [Tree Symbol]) :: [Tree Symbol] where
  AddEdge a trees = AddEdge' a trees 'False 'False

-- | Auxilliary function normally defined in a @where@ clause for manual folding.
type family SpanningTrees' (edges :: [EdgeKind])
                           (acc :: [Tree Symbol]) :: [Tree Symbol] where
  SpanningTrees' '[] trees = trees
  SpanningTrees' (('EdgeType from to) ': es) trees =
    SpanningTrees' es (AddEdge ('EdgeType from to) trees)

-- | Expects edges to already be type-safe
type family SpanningTrees (edges :: [EdgeKind]) :: [Tree Symbol] where
  SpanningTrees edges = SpanningTrees' edges '[]

getSpanningTrees :: EdgeSchema es x unique -> Proxy (SpanningTrees es)
getSpanningTrees _ = Proxy
