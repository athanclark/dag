{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Graph.DAG.Edge where

import Data.Constraint
import GHC.TypeLits
import Data.Proxy

-- | We use promoted symbol values for the @from@ and @to@ type parameters. This
-- is the user-level data type when declaring the list of edges.
data EdgeValue (from :: Symbol) (to :: Symbol) = Edge

-- | We need this for type-level computation list.
data EdgeKind = forall from to. EdgeType from to

-- | Some people just want to watch the world burn. Ideally, this shouldn't
-- exist; poor error messages, and is very square peg - round hole.
type family Deducible (x :: Bool) :: Constraint where
  Deducible 'True = ()

-- | @not . elem@ for lists of types, resulting in a constraint.
type family Excluding (x :: k) (xs :: Maybe [k]) :: Constraint where
  Excluding a ('Just '[]) = Deducible 'True -- Basis
  Excluding a 'Nothing    = Deducible 'True -- Basis
  Excluding a ('Just (a ': ts)) = Deducible 'False -- Reject & Refute
  Excluding a ('Just (b ': ts)) = Excluding a ('Just ts) -- continue

-- | A simple @Data.List.lookup@ function for type maps.
type family Lookup (index :: k) ( map :: [(k, k2)] ) :: Maybe k2 where
  Lookup a ( '( a, v) ': xs ) = 'Just v
  Lookup a (b ': xs) = Lookup a xs
  Lookup a '[] = 'Nothing

-- | Trivial inequality for non-reflexivity of edges
type family (x :: k1) =/= (y :: k2) :: Constraint where
  a =/= a = Deducible 'False
  a =/= b = Deducible 'True

-- | Simply reject anything that's been reached in the other direction. We
-- expect an explicit type signature when uniqueness is needed, otherwise we
-- will wait until invocation to see if the edges are unique.
class Acceptable (a :: EdgeKind)
                 ( oldLoops :: [(Symbol, [Symbol])] )
                 (unique :: Bool) where
instance ( Excluding from (Lookup to excludeMap)
         , from =/= to ) =>
            Acceptable ('EdgeType from to) excludeMap 'False where
instance ( Excluding from (Lookup to excludeMap)
         , Excluding to (Lookup from excludeMap)
         , from =/= to ) =>
            Acceptable ('EdgeType from to) excludeMap 'True where

-- | Add an explicit element to the head of a list, if the test is inside that
-- list.
type family PrependIfElem (test :: k) (a :: k) (xs :: [k]) :: [k] where
  PrependIfElem t a (t ': xs) = a ': t ': xs
  PrependIfElem t a (u ': xs) = u ': (PrependIfElem t a xs)
  PrependIfElem t a '[]       = '[]

-- | Update the exclusion map with the new edge: the @from@ key gets @to@ added,
-- likewise with keys that have @from@ in it's value list. We need to track if
-- the key exists yet.
type family DisallowIn
              (new :: EdgeKind)
              ( oldLoops :: [(Symbol, [Symbol])] )
              (keyFound :: Bool) :: [(Symbol, [Symbol])] where
-- | When `from ~ key`:
  DisallowIn ('EdgeType from to) ( '(from, xs) ': es) 'False =
    '(from, (to ': xs)) ':                      -- add @to@ to transitive reach list
      (DisallowIn ('EdgeType from to) es 'True) -- continue
-- | When `from ~/~ key`, and `from ~/~ head value`
  DisallowIn ('EdgeType from to) ( '(key, vs) ': es ) keyFound =
    '(key, (PrependIfElem from to vs)) ':            -- find the needle if it exists
        (DisallowIn ('EdgeType from to) es keyFound) -- continue
-- | Basis
  DisallowIn a '[] 'True = '[] -- search over.
-- | Growth via append
  DisallowIn ('EdgeType from to) '[] 'False = ('(from, (to ': '[])) ': '[])

-- | @edges@ is a list of types with kind @EdgeKind@, while @nearLoops@ is a
-- map of the nodes transitively reachable by each node.
data EdgeSchema (edges :: [EdgeKind])
                (nearLoops :: [(Symbol, [Symbol])])
                (unique :: Bool) where
  ENil :: EdgeSchema '[] '[] unique
  ECons :: ( Acceptable b oldLoops unique
           , EdgeValue from to ~ a
           , EdgeType from to ~ b
           , DisallowIn b oldLoops 'False ~ c
           ) => !a
             -> EdgeSchema old oldLoops unique
             -> EdgeSchema (b ': old) c unique

-- | Utility for constructing an @EdgeSchema@ granularly
unique :: EdgeSchema '[] '[] 'True
unique = ENil

notUnique :: EdgeSchema '[] '[] 'False
notUnique = ENil

-- | Trivial rose tree for creating spanning trees
data Tree a = Node a [Tree a]

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

  -- | Go downward, and laterally (I think).
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
