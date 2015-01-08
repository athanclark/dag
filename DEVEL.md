Development Notes
=================

Here we take some notes on the struture and design we want to sustain, and
capture in this project.

## Definition

Our DAG will establish and sustain the following properties:

- The graph must be directed
- The graph must be acyclic
- The graph may be disjoint
- The graph may be empty
- Edges must be unique

We also bring the following notions:

- The graph is statically generated from a finite symbol set, and immutable
- Collapsation of the graph to a list should be very simple.

## Overview

We will implement the construction of graphs via an inductive list of edges.

Every `Cons` cell has an `Edge` type parameter that _changes_ depending on it's inner
cells. For instance, if I start with `[]`, then add an Edge from `a` to `b`,
then the "acceptable space" of edges now removes __both__ `a -> b` __and__
`b -> a`, because we demand unique edges and acyclicity.

> TODO: Prove transitivity elimination is achieved with discrete elimination.

> TODO: Discover whether cycle checks are necessary throughout the structure

### Inclusion

Ideally, I would like to simply allow the edge to be of some type __or__ another type, allowing ad-hoc heterogeneous behavior, that way we can encode the edge's "to" and "from" symbols at the type level via `-XDataKinds`, but no such doodad exists in Haskell yet.

#### Ideas

Singletons gives us a mirror image of the value level, at the type level. For instance:

```haskell
{-# LANGUAGE DataKinds #-}

module Foo where

data Nat = Z | S Nat

genSingletons [''Nat]
```

Now we can do this:

```haskell
{-# LANGUAGE KindSignatures #-}

import Foo

foo :: (S::Nat) ((S::Nat) (Z::Nat))
foo = SS (SS SZ)
```

So, the type reflects the value structure in the kind `Nat`, which is useful for phantom types and type families / operators. However, we can promote functions that operate on these types with singletons, and make their mirrored values as well.
