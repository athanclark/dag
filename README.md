dag
===

> Directed Acyclic Graphs for Haskell

## Description

This is a type-safe directed acyclic graph library for Haskell. This library
differs from others in a number of ways:

- Each graph has an alphabet of abstract symbols - to be used for lookups and
  other references
- We allow disjointed graphs in a single instance
- Only unique edges are allowed, as with unique nodes (no duplicates of the
  same symbol)

## Installation

TODO: Write installation instructions here

## Usage

First, we need to make our edge data type:

```haskell
makeEdges'' SymbolSet
```

This creates a `EdgeSymbolSet` type and `EdgeSymbolSet`

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
