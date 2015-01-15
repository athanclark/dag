{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Graph.DAG where

import Data.Graph.DAG.Internal
import Data.Constraint

-- Dummy type for development
data Sym = Foo | Bar | Baz | Qux

-- | We use promoted symbol values for the @from@ and @to@ type parameters.
data EdgeValue (from :: Sym) (to :: Sym) = Edge

-- | We need this for our list of edges.
data EdgeKind = forall from to. EdgeType from to

-- | The basis for the exclusion map.
type EmptyLoops = (  ('Foo, [])
                  ': ('Bar, [])
                  ': ('Baz, [])
                  ': ('Qux, [])
                  ': '[] )

-- | Some people just want to watch the world burn.
type family Deducible x :: Constraint
type instance Deducible 'True = ()

-- | @not . elem@ for lists of types, resulting in a constraint.
type family Excluding (x :: k) (xs :: [k]) :: Constraint
type instance Excluding a '[] = Deducible 'True       -- Basis
type instance Excluding a (a ': ts) = Deducible 'False    -- Reject
type instance Excluding a (b ': ts) = Excluding a ts

-- | A simple @Data.List.lookup@ function for type lists.
type family Lookup
              (index :: Sym)
              ( map :: [(Sym, [Sym])] ) :: [Sym]
type instance Lookup 'Foo ('Foo excludes ': xs) = excludes
type instance Lookup a (b ': xs) = Lookup a xs

-- | Simply reject anything that's present in the list, and accept anything that
-- isnt & stuff. Non-deducability ~ refutation.
type family Acceptable
              (a :: EdgeKind)
              ( oldLoops :: [(Sym, [Sym])] ) :: Constraint
type instance (Excluding 'Foo (Lookup 'Bar excludeMap)) =>
                Acceptable
                  ('EdgeType 'Foo 'Bar)
                  excludeMap = Deducible 'True
type instance Acceptable
                a
                EmptyLoops = Deducible 'True

-- | Update the exclusion map with the new edge
type family (new :: EdgeKind)
              :disallowedIn:
                ( oldLoops :: [(Sym, [Sym])] ) :: [(Sym, [Sym])]
type instance ('EdgeType from to)
                :disallowedIn:
                  -- TODO: Update `from` key, and add `to` to every key that
                  --       has `from` in it's value list.

-- | @edges@ is a list of types with kind @EdgeKind@, while @nearLoops@ is a
-- dictionary of the nodes transitively reachable by the index.
data Dag (edges :: [EdgeKind]) ( nearLoops :: [(Sym, [Sym])] ) where
  GNil :: Dag '[] EmptyLoops
  GCons :: ( Acceptable b oldLoops
           , EdgeValue from to ~ a
           , EdgeType from to ~ b
           ) => a
             -> Dag old oldLoops
             -> Dag (b ': old) (b :disallowedIn: oldLoops)
