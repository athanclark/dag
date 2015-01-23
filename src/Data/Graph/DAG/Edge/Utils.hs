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
import Data.Foldable (Foldable (foldMap))
import Control.Applicative

-- | Trivial rose tree for creating spanning trees. We make control structure
-- instances "parallel" (instead of cartesian) by default for simplicity.
$(singletons [d|
  data RTree a = a :@-> [RTree a] deriving (Show, Eq, Functor)
  |])

instance Applicative RTree where
  pure a = a :@-> []
  (f :@-> fs) <*> (x :@-> xs) = (f x) :@->
    (zipWith (<*>) fs xs)

instance Monad RTree where
  return = pure
  (x :@-> xs) >>= f = case f x of -- Substitution based instance.
    (y :@-> ys) -> y :@-> (fmap (>>= f) xs)

instance Monoid a => Monoid (RTree a) where
  mempty = mempty :@-> []
  (x :@-> xs) `mappend` (y :@-> ys) = (x `mappend` y) :@->
    (zipWith mappend xs ys)

instance Foldable RTree where
  foldMap f (x :@-> xs) = f x <> foldMap (foldMap f) xs

-- | Gives us a generic way to get our spanning trees of the graph, as a value.
-- Credit goes to <http://stackoverflow.com/questions/28030118/reflecting-heterogeneous-promoted-types-back-to-values-compositionally András Kovács>.
reflect ::
  forall (a :: k).
  (SingI a, SingKind ('KProxy :: KProxy k)) =>
  Proxy a -> Demote a
reflect _ = fromSing (sing :: Sing a)

-- | Adds an empty @c@ tree to the list of trees uniquely
type family AppendIfNotElemTrees (c :: k) (trees :: [RTree k]) :: [RTree k] where
  AppendIfNotElemTrees c ((c :@-> xs) ': xss) = (c :@-> xs) ': xss
  AppendIfNotElemTrees c ((x :@-> xs) ': xss) = (x :@-> xs) ':
    (AppendIfNotElemTrees c xss)
  AppendIfNotElemTrees c '[] = (c :@-> '[]) ': '[]

-- | Adds @c@ as a child of any tree with a root @t@. Assumes unique roots.
type family AddChildTo (test :: k)
                       (child :: k)
                       (trees :: [RTree k]) :: [RTree k] where
  AddChildTo t c ((t :@-> xs) ': xss) =
    (t :@-> (AppendIfNotElemTrees c xs)) ': (AddChildTo t c xss)
  AddChildTo t c ((x :@-> xs) ': xss) =
    (x :@-> (AddChildTo t c xs)) ': (AddChildTo t c xss)
  AddChildTo t c '[] = '[]

-- | We need to track if @from@ has is a root node or not. TODO: Some code repeat.
type family AddEdge' (edge :: EdgeKind)
                     (trees :: [RTree Symbol])
                     (hasFromRoot :: Bool)
                     (hasToRoot :: Bool):: [RTree Symbol] where
  AddEdge' ('EdgeType from to) '[] 'False 'False =
    (from :@-> ((to :@-> '[]) ': '[])) ': (to :@-> '[]) ': '[]

  AddEdge' ('EdgeType from to) '[] 'True 'False =
    (to :@->                     '[])  ':                  '[]

  AddEdge' ('EdgeType from to) '[] 'False 'True =
    (from :@-> ((to :@-> '[]) ': '[])) ':                  '[]

  AddEdge' x '[] 'True 'True = '[]

  AddEdge' ('EdgeType from to) ((from :@-> xs) ': xss) hasFromRoot hasToRoot =
    (from :@-> (AppendIfNotElemTrees to xs)) ':
      (AddEdge' ('EdgeType from to) xss 'True hasToRoot)

  AddEdge' ('EdgeType from to) ((to :@-> xs) ': xss) hasFromRoot hasToRoot =
    (to :@-> (AddEdge' ('EdgeType from to) xs 'True 'True)) ':
      (AddEdge' ('EdgeType from to) xss hasFromRoot 'True)

  -- Go downward, and laterally (I think).
  AddEdge' ('EdgeType from to) ((x :@-> xs) ': xss) hasFromRoot hasToRoot =
    (x :@-> (AddEdge' ('EdgeType from to) xs 'True 'True)) ':
      (AddEdge' ('EdgeType from to) xss hasFromRoot hasToRoot)

-- | Add @to@ as a child to every @from@ node in the accumulator.
type family AddEdge (edge :: EdgeKind)
                    (trees :: [RTree Symbol]) :: [RTree Symbol] where
  AddEdge a trees = AddEdge' a trees 'False 'False

-- | Auxilliary function normally defined in a @where@ clause for manual folding.
type family SpanningTrees' (edges :: [EdgeKind])
                           (acc :: [RTree Symbol]) :: [RTree Symbol] where
  SpanningTrees' '[] trees = trees
  SpanningTrees' (('EdgeType from to) ': es) trees =
    SpanningTrees' es (AddEdge ('EdgeType from to) trees)

-- | Expects edges to already be type-safe
type family SpanningTrees (edges :: [EdgeKind]) :: [RTree Symbol] where
  SpanningTrees edges = SpanningTrees' edges '[]

getSpanningTrees :: EdgeSchema es x unique -> Proxy (SpanningTrees es)
getSpanningTrees _ = Proxy

-- | Get the spanning trees of an @EdgeSchema@. Operate on the assumtion that
-- the data returned is actually @[Tree String]@.
espanningtrees :: SingI (SpanningTrees' es '[]) =>
                  EdgeSchema es x unique
               -> Demote (SpanningTrees' es '[])
espanningtrees = reflect . getSpanningTrees

-- | Get a single tree.
etree :: SingI (SpanningTrees' es '[]) =>
         String -> EdgeSchema es x unique -> Maybe (RTree String)
etree k es = getTree k $ espanningtrees es
  where
  getTree k1 ( n@(k2 :@-> xs) : ns ) | k1 == k2 = Just n
                                     | otherwise = getTree k1 ns
  getTree _ [] = Nothing

-- | Degenerate (but type-safe!) @head@.
ehead :: ( EdgeType from to ~ b
         , EdgeValue from to ~ a
         ) => EdgeSchema (b ': old) c u -> a
ehead _ = Edge

-- | For now, we only suport unique edges.
eTreeToEdges :: RTree String -> [(String,String)]
eTreeToEdges = treeToEdges' []
  where
  treeToEdges' :: [(String,String)]
               -> RTree String
               -> [(String,String)]
  treeToEdges' zs (_ :@-> []) = zs
  treeToEdges' zs (x :@-> xs) =
    let newEdges = umerge zs $ map (\q -> (x, getNodeVal q)) xs
    in
    foldl treeToEdges' newEdges xs
  getNodeVal (x :@-> _) = x
  -- unique merge
  umerge [] ys = ys
  umerge (x:xs) ys | x `elem` ys = umerge xs ys
                   | otherwise = x : umerge xs ys

-- | Get a first-class list of edges from spanning trees. Only works on uniqely
-- edged @EdgeSchema@'s.
eForestToEdges :: [RTree String] -> [(String,String)]
eForestToEdges xs = foldl (\es t -> umerge es $ eTreeToEdges t) [] xs
  where
  -- unique merge
  umerge [] ys = ys
  umerge (x:xs) ys | x `elem` ys = umerge xs ys
                   | otherwise = x : umerge xs ys

-- | Get the "First-Class" edges of a uniquely-edged @EdgeSchema@.
fcEdges :: SingI (SpanningTrees' es '[]) =>
           EdgeSchema es x 'True -> [(String, String)]
fcEdges = eForestToEdges . espanningtrees

-- eflip e = espanningtrees e
