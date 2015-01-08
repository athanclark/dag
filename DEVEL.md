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
- A finite, flat symbol set will denote nodes uniquely (a `*` kinded data type without any composite values - we inspect via Template Haskell)

We also bring the following notions:

- The graph is statically generated from a finite symbol set, and immutable
- Collapsation of the graph to a list should be very simple.
- The Graph is __Concrete__ - meaning it's not higher kinded. We expect use of this graph to be only in rendered-form, where using the graph for arbitrary types means you render the graph into a list of symbol values, then map a function that pattern matches on the value cases to give the real value you want.

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

##### Usage

Here might be a way to define graphs:

```haskell
data Dag Available where
  GNil :: Dag All
  GCons :: (from,to) :satisfies: c =>
           (from::Sym) :edge: (to::Sym)
        -> Dag c
        -> Dag (a :into: c))
```

Where `Edge` is a data type, `Sym` is the promoted type (used as a kind) of the finite symbol set denoting the unique nodes. We use a (directed) product in the left-hand-side of the type family / operator `:satisfies:` to denote that the proposed `Edge` will be allowed in the context `c`. That's where the `Available` comes in - as the memory for how we've built the graph so far.

> I _think_ in production, this will just be a dictionary table that gets recursively elements eliminated based on their transitive relations. I _really_ want it to be correct for each discrete addition, though, as alluded to earler.

Lastly, `:into:` is like a `Cons` for our incremental type checker / enforcement system.

Also, `to` and `from` are our singleton values

##### Rendering

We need to fold over the graph's edge listing, adding each entry to an accumulator of (untyped) spanning trees. I say "untyped" because we've already done the acyclicity and duplication checks from our static type - this tree will now be vulnerable.

We then do recursive, single-time, bredth-first `get`'s on the Graph. This means, we first design our graph intuitively, drawing edges _toward_ dependencies. Then, we flip all edges and take the top-level spainning rose trees, where each set of children are just parsed linearly, and if the child hasen't already been appended to the list, then we're good. Then, the resulting list will have the most distant nodes at the head of the list.

```haskell
graphToList :: Dag c -> [Sym]
graphToList GNil = []
graphToList (GCons ) -- ...TODO.

dependenciesFirst :: Dag c -> [Sym]
dependenciesFirst = graphToList . flipEdges
```
