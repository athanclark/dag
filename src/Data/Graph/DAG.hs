{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Graph.DAG where

import Data.Graph.DAG.Internal
import Data.Constraint

data AllowedEdges = All        -- FIXME
                  | Except (Edge () ()) AllowedEdges

data Edge from to = Edge from to

type family Into a (c :: AllowedEdges) :: AllowedEdges

type family Satis a (c :: AllowedEdges) :: Constraint

data Dag (context :: AllowedEdges) where
  GNil :: Dag context
  GCons :: ( Satis (from,to) c
           , Edge from to ~ a
           ) => a
             -> Dag c
             -> Dag (Into a c)
