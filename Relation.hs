-- |

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}


module Relation where

import Prelude hiding ((>))
import Category
import Premonoidal
import Morphism
import Data.Finitary
import Data.Void
import Data.Set
import Function
import Any
import GHC.Generics (Generic)

-- Category of relations
newtype Relation a b = Relation {getRelation :: [(a,b)]}

instance Category Relation Finitary where
  identity = Relation [(a,a) | a <- inhabitants]
  composition = Relation [()]

-- -- The monoidal category of relations over a quantale.
-- relationLift :: (Finitary a, Finitary b, Quantale q) => Function a b -> Relation q a b
-- relationLift (Function f) = Relation (\a b -> if f a == b then qunit else qempty)

-- relationColift :: (Finitary a, Finitary b, Quantale q) => Function b a -> Relation q a b
-- relationColift (Function f) = Relation (\a b -> if a == f b then qunit else qempty)

-- instance (Quantale q) => Premonoidal (Relation q) Finitary (,) () where
--   alpha = relationLift alpha
--   alphainv = relationLift alphainv
--   rho = relationLift rho
--   rhoinv = relationLift rhoinv
--   lambda = relationLift lambda
--   lambdainv = relationLift lambdainv
--   tensor (Relation r) (Relation s) = Relation $
--     \(a1,a2) (b1,b2) -> r a1 b1 `qmult` s a2 b2



-- -- | Boolean quantale.
-- instance Quantale Bool where
--   qunit = True
--   qmult = (&&)
--   qjoin f = foldr (||) qunit [ f x | x <- inhabitants ]
-- type BooleanRel a b = Relation Bool a b

-- -- | Natural numbers quantale.
-- instance Quantale Integer where
--   qunit = 0
--   qmult = (*)
--   qjoin f = foldr (+) qunit [ f x | x <- inhabitants ]
-- type FiniteSpan a b = Relation Integer a b



-- -- Example: relations
-- type Rel = Morphism (Relation Bool) (,) ()
-- data Person = Alice | Bob deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)

-- alice :: Rel '[] '[Person]
-- bob :: Rel '[] '[Person]
-- loves :: Rel '[] '[Person,Person]
-- alice = Morphism $ relationLift (Function (\() -> (Alice,())))
-- bob = Morphism $ relationLift (Function (\() -> (Bob,())))
-- loves = Morphism $ relationLift (Function (\() -> (Alice,(Bob,()))))

-- cup :: (Finitary a) => Rel '[a,a] '[]
-- cup = Morphism $ Relation $ \(x,(y,())) () -> x == y

-- check :: Rel '[] '[] -> Bool
-- check (Morphism (Relation f)) = f () ()

-- discocat :: Rel '[] '[]
-- discocat =
--   ((>))  (alice |*| loves |*| bob)
--    |>|   (      cup  |*|  cup    )

-- type Spans = Morphism (Relation Integer) (,) ()
-- nor :: Spans '[Bool,Bool] '[Bool]
-- nor = Morphism .relationLift.Function $ \(a,(b,())) -> (not (or [a,b]),())
-- copy :: Spans '[Bool] '[Bool,Bool]
-- copy = Morphism .relationLift.Function $ \(a,()) -> (a,(a,()))
-- cocopy :: Spans '[Bool,Bool] '[Bool]
-- cocopy = Morphism .relationColift.Function $ \(a,()) -> (a,(a,()))
-- discard :: Spans '[Bool] '[]
-- discard = Morphism .relationLift.Function $ \(a,()) -> ()
-- codiscard :: Spans '[] '[Bool]
-- codiscard = Morphism .relationColift.Function $ \(a,()) -> ()
-- wire :: Finitary a => Spans '[a] '[a]
-- wire = Morphism identity
-- swap :: (Finitary a, Finitary b) => Spans '[a,b] '[b,a]
-- swap = Morphism .relationLift.Function $ \(a,(b,())) -> (b,(a,()))

-- diagramSpan :: Spans '[Bool] '[Bool]
-- diagramSpan =
--   ((>))  codiscard  |*|  wire
--    |>|      copy    |*|  wire
--    |>|   wire  |*|  nor
--    |>|   wire  |*|  copy
--    |>|      cocopy  |*|  wire
--    |>|      discard |*|  wire

-- norLatch :: Spans '[Bool,Bool] '[Bool,Bool]
-- norLatch =
--   ((>)) wire |*| codiscard |*| codiscard |*| wire
--    |>|  wire |*|   copy    |*|   copy    |*| wire
--    |>|       nor     |*| swap  |*|       nor
--    |>|    copy  |*| wire   |*|  wire |*|  copy
--    |>|  wire |*| cocopy    |*|   cocopy |*|  wire
--    |>|  wire |*| discard   |*|  discard |*|  wire

