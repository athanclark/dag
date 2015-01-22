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
{-# LANGUAGE FunctionalDependencies #-}

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
              (keyFoundYet :: Bool) :: [(Symbol, [Symbol])] where
-- When @from ~ key@:
  DisallowIn ('EdgeType from to) ( '(from, xs) ': es) 'False =
    '(from, (to ': xs)) ':                      -- add @to@ to transitive reach list
      (DisallowIn ('EdgeType from to) es 'True) -- continue
-- When @from ~/~ key@, and @from ~/~ head value@
  DisallowIn ('EdgeType from to) ( '(key, vs) ': es ) keyFoundYet =
    '(key, (PrependIfElem from to vs)) ':            -- find the needle if it exists
        (DisallowIn ('EdgeType from to) es keyFoundYet) -- continue
-- Basis
  DisallowIn a '[] 'True = '[] -- search over.
-- Growth via append
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
             -> !(EdgeSchema old oldLoops unique)
             -> EdgeSchema (b ': old) c unique

-- | Utility for constructing an @EdgeSchema@ incrementally without a type
-- signature.
unique :: EdgeSchema '[] '[] 'True
unique = ENil

notUnique :: EdgeSchema '[] '[] 'False
notUnique = ENil
