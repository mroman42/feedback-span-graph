-- | Coherence for Monoidal categories

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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module UnbiasedMonoidalHomInstances where


import UnbiasedMonoidalHom
import Category
import Premonoidal
import Data.Finitary
import Data.Maybe
import Data.Finite
import Numeric.Natural
import GHC.Generics (Generic)
import Prelude hiding ((>))



-- EXAMPLES
type Sets = Morphism Function (,) ()


diagram :: Sets '[Int,Int,Int,Int] '[Int]
diagram =
       plus  |*|  plus
  |>|       plus

plus :: Sets '[Int,Int] '[Int]
plus = Morphism $ Function $ (\(a,(b,())) -> (a + b, ()))
  -- unsafeSkew @(,) @()
  -- .(uncurry (+) :: (Int,Int) -> Int)
  -- .unsafeunSkew @(,) @()

type IOEffect = Morphism (Kleisli IO) (,) ()

example :: IOEffect '[] '[]
example =
  ((>)) putString "hello, "  |*| putString "world!"

putString :: String -> IOEffect '[] '[]
putString s = Morphism $ Kleisli $ (\() -> putStrLn s)


-- Example: relations
type Relations = Morphism Relation (,) ()
data Person = Alice | Bob deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
alice :: Relations '[] '[Person]
alice = Morphism $ relationLift (Function (\() -> (Alice,())))
bob :: Relations '[] '[Person]
bob = Morphism $ relationLift (Function (\() -> (Bob,())))
loves :: Relations '[] '[Person,Person]
loves = Morphism $ relationLift (Function (\() -> (Alice,(Bob,()))))
cup :: (Finitary a) => Relations '[a,a] '[]
cup = Morphism $ Relation $ \(x,(y,())) () -> x == y
check :: Relations '[] '[] -> Bool
check (Morphism (Relation f)) = f () ()

discocat :: Relations '[] '[]
discocat =
  ((>))  (alice |*| loves |*| bob)
   |>|   (      cup  |*|  cup    )

-- Example: spans
type Spans = Morphism Span (,) ()
nor :: Spans '[Bool,Bool] '[Bool]
nor = Morphism .spanLift.Function $ \(a,(b,())) -> (not (or [a,b]),())
copy :: Spans '[Bool] '[Bool,Bool]
copy = Morphism .spanLift.Function $ \(a,()) -> (a,(a,()))
cocopy :: Spans '[Bool,Bool] '[Bool]
cocopy = Morphism .spanColift.Function $ \(a,()) -> (a,(a,()))
discard :: Spans '[Bool] '[]
discard = Morphism .spanLift.Function $ \(a,()) -> ()
codiscard :: Spans '[] '[Bool]
codiscard = Morphism .spanColift.Function $ \(a,()) -> ()
wire :: Finitary a => Spans '[a] '[a]
wire = Morphism identity
swap :: (Finitary a, Finitary b) => Spans '[a,b] '[b,a]
swap = Morphism .spanLift.Function $ \(a,(b,())) -> (b,(a,()))


diagramSpan :: Spans '[Bool] '[Bool]
diagramSpan =
  ((>))  codiscard  |*|  wire
   |>|      copy    |*|  wire
   |>|   wire  |*|  nor
   |>|   wire  |*|  copy
   |>|      cocopy  |*|  wire
   |>|      discard |*|  wire

norLatch :: Spans '[Bool,Bool] '[Bool,Bool]
norLatch =
  ((>)) wire |*| codiscard |*| codiscard |*| wire
   |>|  wire |*|   copy    |*|   copy    |*| wire
   |>|       nor     |*| swap  |*|       nor
   |>|    copy  |*| wire   |*|  wire |*|  copy
   |>|  wire |*| cocopy    |*|   cocopy |*|  wire
   |>|  wire |*| discard   |*|  discard |*|  wire
