dag
===

> Directed Acyclic Graphs for Haskell

## Description

This is a type-safe directed acyclic graph library for Haskell. This library
differs from others in a number of ways:

- Edge construction is inductive, creating a "schema":

  ```haskell
  {-# LANGUAGE DataKinds #-}

  import Data.Graph.DAG.Edge

  -- | Edges are statically defined:
  edges = ECons (Edge :: EdgeValue "foo" "bar") $
    ECons (Edge :: EdgeValue "bar" "baz") $
    ECons (Edge :: EdgeValue "foo" "baz")
    unique -- ENil, but for uniquely edged graphs
  ```

- Which we use to inductively populate nodes with values:

  ```haskell
  data Cool = AllRight
            | Radical
            | SuperDuper

  graph = GCons "foo" AllRight $
    GCons "bar" Radical $
    GCons "baz" SuperDuper $
    GNil edges
  ```

  It's an instance of `Functor`, but we haven't done much here - it will require
  a lot of reflection that I don't have time to implement right now - there isn't
  even binding of value-based `GCons` keys and `ECons` edge node labels.

- Lots of type-level computation:

  ```haskell
  *Data.Graph.DAG> :t edges
  edges
    :: EdgeSchema
         '['EdgeType "foo" "bar", 'EdgeType "bar" "baz",
           'EdgeType "foo" "baz"] -- Type list of edges
         '['("foo", '["bar", "baz"]), '("bar", '["baz"])] -- potential loops
         'True -- uniqueness
  ```

  ```haskell
  *Data.Graph.DAG> :t getSpanningTrees $ edges
  getSpanningTrees $ edges
    :: Data.Proxy.Proxy
         '['Node "foo" '['Node "bar" '['Node "baz" '[]],
                         'Node "baz" '[]],
           'Node "bar" '['Node "baz" '[]],
           'Node "baz" '[]]
  ```

This library is still very naive, but it will give us compile-time enforcement
of acyclicity (and uniqueness) in these graphs - ideal for dependency graphs.

## Usage

You will need `-XDataKinds` for the type-level symbols:

```haskell
{-# LANGUAGE DataKinds #-}

import Data.Graph.DAG
import GHC.TypeLits

...
```
