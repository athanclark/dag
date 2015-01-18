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

module Data.Graph.DAG where

import Data.Graph.DAG.Internal
import Data.Constraint

-- Dummy type for development
data Sym = Foo | Bar | Baz | Qux

-- | We use promoted symbol values for the @from@ and @to@ type parameters.
data EdgeValue (from :: Sym) (to :: Sym) = Edge

-- | We need this for our list of edges.
data EdgeKind = forall from to. EdgeType from to

-- | Some people just want to watch the world burn.
type family Deducible (x :: Bool) :: Constraint where
  Deducible 'True = ()

-- | @not . elem@ for lists of types, resulting in a constraint.
type family Excluding (x :: k) (xs :: [k]) :: Constraint where
  Excluding a '[] = Deducible 'True        -- Basis
  Excluding a (a ': ts) = Deducible 'False -- Reject & Refute
  Excluding a (b ': ts) = Excluding a ts

-- | A simple @Data.List.lookup@ function for type lists.
type family Lookup (index :: k) ( map :: [(k, k2)] ) :: k2 where
  Lookup a ( '( a, v) ': xs ) = v
  Lookup a (b ': xs) = Lookup a xs

-- | Simply reject anything that's present in the list, and accept anything that
-- isnt & stuff. Non-deducability / non-instance ~ refutation.
-- FIXME: No support for unique edges.
class Acceptable (a :: EdgeKind) ( oldLoops :: [(Sym, [Sym])] ) where
instance (Excluding from (Lookup to excludeMap)) =>
            Acceptable ('EdgeType from to) excludeMap where

-- | Add an explicit element to the head of a list, if the test is inside that
-- list.
type family PrependIfElem (test :: k) (a :: k) (xs :: [k]) :: [k] where
  PrependIfElem t a (t ': xs) = a ': t ': xs
  PrependIfElem t a (u ': xs) = u ': (PrependIfElem t a xs)
  PrependIfElem t a '[]       = '[]

-- | Update the exclusion map with the new edge: the @from@ key gets @to@ added,
-- likewise with @from@ in it's value list.
type family DisallowedIn
              (new :: EdgeKind)
              ( oldLoops :: [(Sym, [Sym])] ) :: [(Sym, [Sym])] where
-- | When `from ~ key`:
  DisallowedIn ('EdgeType from to) ( '(from, xs) ': es) =
    '(from, (to ': xs)) ': -- add @to@ to transitive reach list
      (DisallowedIn ('EdgeType from to) es) -- continue
-- | When `from ~/~ key`, and `from ~/~ head value`
  DisallowedIn  ('EdgeType from to) ( '(key, vs) ': es ) =
    '(key, (PrependIfElem from to vs)) ': -- find the needle if it exists
        (DisallowedIn ('EdgeType from to) es) -- continue
-- | Basis
  DisallowedIn a '[] = '[] -- search over.

-- | @edges@ is a list of types with kind @EdgeKind@, while @nearLoops@ is a
-- map of the nodes transitively reachable by each node.
data Dag (edges :: [EdgeKind]) ( nearLoops :: [(Sym, [Sym])] ) where
  GNil :: Dag '[] ('( 'Foo, '[] )
                    ': '( 'Bar, '[] )
                      ': '( 'Baz, '[] )
                        ': '( 'Qux, '[] ) ': '[])
  GCons :: ( Acceptable b oldLoops
           , EdgeValue from to ~ a
           , EdgeType from to ~ b
           , DisallowedIn b oldLoops ~ c
           ) => !a
             -> Dag old oldLoops
             -> Dag (b ': old) c
