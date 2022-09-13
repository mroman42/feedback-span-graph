-- | State.hs

-- The state construction over a premonoidal category. We wrote a definition of
-- the construction in "A Canonical Algebra of Open Transition Systems".

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

module State where

import Prelude hiding ((>))
import Morphism
import Category
import Premonoidal
import Translate
import BellsAndWhistles


-- State construction.
data State hom obj o i x y where
  State :: (Premonoidal hom obj o i, obj s)
    => hom (s `o` x) (s `o` y) -> State hom obj o i x y

-- Identity and composition.
identityState :: forall hom obj o i x . (Premonoidal hom obj o i, obj x) => State hom obj o i x x
identityState = State $ tensor (identity @hom @obj @i) identity

compositionState :: (SymmetricPremonoidal hom obj o i, obj x, obj y, obj z)
  => State hom obj o i x y -> State hom obj o i y z -> State hom obj o i x z
compositionState (State f) (State g) = State $ toAAoABBoB $
  (>)      swap |*| wire
  |>|  wire |*| fromAABB f
  |>|      swap |*| wire
  |>|  wire |*| fromAABB g

tensorState :: (SymmetricPremonoidal hom obj o i,
                obj x1, obj x2, obj y1, obj y2)
  => State hom obj o i x1 y1 -> State hom obj o i x2 y2
  -> State hom obj o i (x1 `o` x2) (y1 `o` y2)
tensorState (State f) (State g) = State $ toAAoAABBoBB $
  (>)   wire |*| swap |*| wire
  |>|  fromAABB f |*| fromAABB g
  |>|   wire |*| swap |*| wire

-- | The state construction forms a symmetric premonoidal category.
instance (SymmetricPremonoidal hom obj o i) => Category (State hom obj o i) obj where
    identity = identityState
    composition = compositionState

liftState :: (SymmetricPremonoidal hom obj o i, obj x, obj y) => hom x y -> State hom obj o i x y
liftState f = State
  $ composition lambda
  $ composition f
  $ lambdainv

instance (SymmetricPremonoidal hom obj o i) => Premonoidal (State hom obj o i) obj o i where
  alpha = liftState alpha
  alphainv = liftState alphainv
  rho = liftState rho
  rhoinv = liftState rhoinv
  lambda = liftState lambda
  lambdainv = liftState lambdainv
  tensor = tensorState

instance (SymmetricPremonoidal hom obj o i) => SymmetricPremonoidal (State hom obj o i) obj o i where
  sigma = liftState sigma

instance (SymmetricPremonoidal hom obj o i, Freyd hom obj o i)
    => Freyd (State hom obj o i) obj o i where
  lift = liftState . lift

instance (SymmetricPremonoidal hom obj o i, Cofreyd hom obj o i)
    => Cofreyd (State hom obj o i) obj o i where
  colift = liftState . colift


-- | The state construction has feedback.
feedback ::
  forall hom obj o i t xs ys .
  (SymmetricPremonoidal hom obj o i, obj t, FiniteList obj xs, FiniteList obj ys) =>
  Morphism (State hom obj o i) o i (t ': xs) (t ': ys) ->
  Morphism (State hom obj o i) o i xs ys
feedback = witFeedback (layObj @hom @obj @o @i @xs) (layObj @hom @obj @o @i @ys)
  where
    witFeedback :: (SymmetricPremonoidal hom obj o i, obj t, FiniteList obj xs, FiniteList obj ys) =>
      Wit obj (Lay o i xs) ->
      Wit obj (Lay o i ys) ->
      Morphism (State hom obj o i) o i (t ': xs) (t ': ys) ->
      Morphism (State hom obj o i) o i xs ys
    witFeedback Wit Wit (Morphism (State f)) = Morphism . State
      $ composition alphainv
      $ composition f alpha

delay ::
  (SymmetricPremonoidal hom obj o i, obj x) =>
  Morphism (State hom obj o i) o i '[x] '[x]
delay = feedback swap
