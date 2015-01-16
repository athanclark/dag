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
type family Lookup (index :: Sym) ( map :: [(Sym, [Sym])] ) :: [Sym] where
  Lookup a ( '( a, excludes ) ': xs ) = excludes
  Lookup a (b ': xs) = Lookup a xs

-- | Simply reject anything that's present in the list, and accept anything that
-- isnt & stuff. Non-deducability / non-instance ~ refutation.
class Acceptable (a :: EdgeKind) ( oldLoops :: [(Sym, [Sym])] ) where
instance (Excluding from (Lookup to excludeMap)) =>
            Acceptable ('EdgeType from to) excludeMap where

{-
instance Acceptable a (  '( 'Foo, '[] )
          ': '( 'Bar, '[] )
            ': '( 'Baz, '[] )
              ': '( 'Qux, '[] )
                ': '[] ) where
-}

type family AppendIfEqual (test :: Sym) (add :: Sym) (xs :: [Sym]) :: [Sym] where
  AppendIfEqual t a (t ': xs) = a ': t ': xs
  AppendIfEqual t a (u ': xs) = u ': (AppendIfEqual t a xs)
  AppendIfEqual t a '[] = '[]

-- | Update the exclusion map with the new edge: the @from@ key gets @to@ added,
-- and others with @from@ in it's value list will also get @to@.
type family DisallowedIn (new :: EdgeKind) ( oldLoops :: [(Sym, [Sym])] ) :: [(Sym, [Sym])] where
-- | When `from ~ key`:
  DisallowedIn ('EdgeType from to) ( '(from, xs) ': es) =
    '(from, (to ': xs)) ': (DisallowedIn ('EdgeType from to) es) -- recurse down
-- | When `from ~/~ key`, but `from ~ head value`
-- FIXME: appending to an existing potential loop (note this is a list, not a
--        unique list, so not 100% type safe :\)
  DisallowedIn ('EdgeType from to) ( '(key, (from ': xs)) ': es ) =
    ( '(key, (to ': from ': xs)) ': (DisallowedIn ('EdgeType from to) es) )
-- | When `from ~/~ key`, and `from ~/~ head value`
  DisallowedIn  ('EdgeType from to) ( '(key, (x ': xs)) ': es ) =
    '(key, (x ': (AppendIfEqual from to xs))) ':
        (DisallowedIn ('EdgeType from to) es)
-- | Basis
  DisallowedIn a '[] = '[]

-- | @edges@ is a list of types with kind @EdgeKind@, while @nearLoops@ is a
-- dictionary of the nodes transitively reachable by the index.
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
{-
instance Show (Dag edges nearLoops) where
  show GNil = "[]"
  show (GCons (edge :: EdgeValue 'Foo 'Bar) xs) = "(foo -> bar) : " ++
    show xs
-}
