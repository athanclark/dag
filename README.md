dag
===

> Directed Acyclic Graphs for Haskell

## Description

This is a type-safe directed acyclic graph library for Haskell. This library
differs from others in a number of ways:

- Edge construction is incremental, creating a "schema":

  ```haskell
  {-# LANGUAGE DataKinds #-}

  import Data.Graph.DAG.Edge

  -- | Edges are statically defined:
  edges = ECons (Edge :: EdgeValue "foo" "bar") $
    ECons (Edge :: EdgeValue "bar" "baz") $
    ECons (Edge :: EdgeValue "foo" "baz")
    unique -- ENil, but casted for uniquely edged graphs
  ```

- The nodes are separate from edges; graph may be not connected:

  ```haskell
  data Cool = AllRight
            | Radical
            | SuperDuper

  graph = GCons "foo" AllRight $
    GCons "bar" Radical $
    GCons "baz" SuperDuper $
    GNil edges
  ```

- Type safety throughout edge construction:

  ```haskell
  *Data.Graph.DAG> :t edges
  edges
    :: EdgeSchema
         '['EdgeType "foo" "bar", 'EdgeType "bar" "baz",
           'EdgeType "foo" "baz"] -- Type list of edges
         '['("foo", '["bar", "baz"]), '("bar", '["baz"])] -- potential loops
         'True -- uniqueness
  ```

- Various type-level computation

  ```haskell
  *Data.Graph.DAG> :t getSpanningTrees $ edges
  getSpanningTrees $ edges
    :: Data.Proxy.Proxy
         '['Node "foo" '['Node "bar" '['Node "baz" '[]],
                         'Node "baz" '[]],
           'Node "bar" '['Node "baz" '[]],
           'Node "baz" '[]]

  *Data.Graph.DAG> reflect $ getSpanningTrees $ edges
  [Node "foo" [Node "bar" [Node "baz" []]
              ,Node "baz" []]
  ,Node "bar" [Node "baz" []]
  ,Node "baz" []]
  ```

This library is still very naive, but it will give us compile-time enforcement
of acyclicity in these graphs - ideal for dependency graphs.

## Usage

You will need `-XDataKinds` for the type-level symbols:

```haskell
{-# LANGUAGE DataKinds #-}

import Data.Graph.DAG
import GHC.TypeLits

...
```
