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

-- | Simply reject anything that's present in the list, and accept anything that
-- isnt & stuff.
type family Acceptable (a :: EdgeKind) (old :: [EdgeKind]) :: Constraint
type instance Acceptable ('EdgeType 'Foo 'Bar) (('EdgeType 'Baz 'Bar) ': '[]) = ()
type instance Acceptable ('EdgeType 'Foo 'Bar) (('EdgeType 'Foo 'Bar) ': '[]) = () ~ Bool

-- | @context@ is a list of types with kind @EdgeAgain@.
data Dag (context :: [EdgeKind]) where
  GNil :: Dag '[]
  GCons :: ( Acceptable b old
           , EdgeValue from to ~ a
           , EdgeType from to ~ b
           ) => a
             -> Dag old
             -> Dag (b ': old)
