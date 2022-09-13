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


module Span where

import Prelude hiding ((>))
import Category
import Premonoidal
import Morphism
import Data.Finitary
import Data.Void
import Data.Set
import State
import Function
import Any
import BellsAndWhistles
import GHC.Generics (Generic)


-- Category of spans
newtype Span' a b = Span' {getSpan' :: [(a,b)]}

instance Category Span' Finitary where
  identity = Span' [(a,a) | a <- inhabitants]
  composition (Span' r) (Span' s) = Span' [(a,c) | (a,b1) <- r , (b2,c) <- s , b1 == b2]

spanLift :: (Finitary a, Finitary b) => Function a b -> Span' a b
spanLift (Function f) = Span' [(a, f a) | a <- inhabitants]

spanColift :: (Finitary a, Finitary b) => Function b a -> Span' a b
spanColift (Function f) = Span' [(f b, b) | b <- inhabitants]

instance Premonoidal Span' Finitary (,) () where
  alpha = spanLift alpha
  alphainv = spanLift alphainv
  rho = spanLift rho
  rhoinv = spanLift rhoinv
  lambda = spanLift lambda
  lambdainv = spanLift lambdainv
  tensor (Span' r) (Span' s) = Span' [ ((x1,x2),(y1,y2)) | (x1,y1) <- r , (x2,y2) <- s]

instance SymmetricPremonoidal Span' Finitary (,) () where
  sigma = spanLift $ Function (\(a,b) -> (b,a))

instance Freyd Span' Finitary (,) () where
  lift = spanLift . Function
instance Cofreyd Span' Finitary (,) () where
  colift = spanColift . Function

type Span = Morphism Span' (,) ()

diagramSpan :: Span '[Bool] '[Bool]
diagramSpan =
  ((>))  codiscard  |*|  wire
   |>|      copy    |*|  wire
   |>|   wire  |*|  nor
   |>|   wire  |*|  copy
   |>|      cocopy  |*|  wire
   |>|      discard |*|  wire

norLatchSpan :: Span '[Bool,Bool] '[Bool,Bool]
norLatchSpan =
  ((>)) wire |*| codiscard |*| codiscard |*| wire
   |>|  wire |*|   copy    |*|   copy    |*| wire
   |>|       nor     |*| swap  |*|       nor
   |>|    copy  |*| wire   |*|  wire |*|  copy
   |>|  wire |*| cocopy    |*|   cocopy |*|  wire
   |>|  wire |*| discard   |*|  discard |*|  wire


