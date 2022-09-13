-- | Coherence for Monoidal categories

-- An alternative is to use Arrow Do notation. However, arrow do notation is
-- restricted to Set-based Freyd categories; while our notation is valid in any
-- premonoidal category. This is not a real restriction, we could easily extend
-- Arrow do to arbitrary premonoidals, but as of 2021, this is not yet
-- implemented.

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

module Morphism where

import Data.Constraint
import Category
import Premonoidal
import Prelude hiding ((>))



(>) :: x -> x
(>) = id

infixl 8 |*|
(|*|) :: forall hom obj o i y1 x2 y2 x1 .
  (Premonoidal hom obj o i,
   FiniteList obj x1, FiniteList obj y1, FiniteList obj x2, FiniteList obj y2) =>
   Morphism hom o i x1 y1 -> Morphism hom o i x2 y2 -> Morphism hom o i (x1 :++: x2) (y1 :++: y2)
(|*|) (Morphism f) (Morphism g) = Morphism $ morphism
    (layObj @hom @obj @o @i @x1) (layObj @hom @obj @o @i @x2) (layObj @hom @obj @o @i @y1) (layObj @hom @obj @o @i @y2)
    (layAppend @hom @obj @o @i @x1 @x2) (layAppend @hom @obj @o @i @y1 @y2)
  where
      morphism ::
            Wit obj (Lay o i x1) -> Wit obj (Lay o i x2) -> Wit obj (Lay o i y1) -> Wit obj (Lay o i y2)
        -> Wit obj (Lay o i (x1 :++: x2)) -> Wit obj (Lay o i (y1 :++: y2))
        -> hom (Lay o i (x1 :++: x2)) (Lay o i (y1 :++: y2))
      morphism Wit Wit Wit Wit Wit Wit = -- making a compiler happy
             laytensorBackward @hom @obj @o @i @x1 @x2
        |<|  tensor f g
        |<|  laytensorForward @hom @obj @o @i @y1 @y2

infixl 7 |>|
(|>|) :: forall hom obj o i x y z.
  (Premonoidal hom obj o i, FiniteList obj x, FiniteList obj y, FiniteList obj z) =>
  Morphism hom o i x y -> Morphism hom o i y z -> Morphism hom o i x z
(|>|) (Morphism f) (Morphism g) = Morphism
    $ morphism (layObj @hom @obj @o @i @x) (layObj @hom @obj @o @i @y) (layObj @hom @obj @o @i @z)
  where
    morphism :: Wit obj (Lay o i x) -> Wit obj (Lay o i y) -> Wit obj (Lay o i z)
      -> hom (Lay o i x) (Lay o i z)
    morphism Wit Wit Wit = composition @hom @obj f g


type family Lay (o :: * -> * -> *) (i :: *) (list :: [*]) :: * where
  Lay o i '[] = i
  Lay o i (x ': xs) = x `o` Lay o i xs

type family (x :: [*]) :++: (y :: [*]) :: [*] where
   '[]      :++: y = y
   (a ': x) :++: y = a ': (x :++: y)
infixr 5 :++:



-- Induction on finite lists of objects.
class FiniteList obj (list :: [*]) where
  induction ::
        t obj '[]
    -> (forall a y . (obj a, FiniteList obj y) => t obj y -> t obj (a ': y))
    ------------------------------------------------------------------------
    -> t obj list

instance FiniteList obj '[] where
  induction z _ = z
instance forall obj a x . (obj a, FiniteList obj x) => FiniteList obj (a ': x) where
  induction z s = s (induction z s)


data Wit c (t :: *) where Wit :: c t => Wit c t


-- Lemma 1. The tensor of a finite list of objects of the monoidal category is
-- an object of the monoidal category.
data LayObj o i obj x where LayObj :: Wit obj (Lay o i x) -> LayObj o i obj x
getLayObj :: forall obj o i x . LayObj o i obj x -> Wit obj (Lay o i x)
getLayObj (LayObj w) = w
layObj :: forall hom obj o i x . (Premonoidal hom obj o i, FiniteList obj x) => Wit obj (Lay o i x)
layObj = getLayObj @obj @o @i @x $ induction basecase generalcase
  where
    basecase :: LayObj o i obj '[]
    basecase = LayObj Wit
    generalcase :: (obj a, FiniteList obj y) => LayObj o i obj y -> LayObj o i obj (a ': y)
    generalcase (LayObj Wit) = LayObj Wit

-- Lemma 2. The tensor of the concatenation of two finite lists of objects of
-- the monoidal category is an object of the monoidal category.
data LayAppend o i y obj x where LayAppend :: Wit obj (Lay o i (x :++: y)) -> LayAppend o i y obj x
getLayAppend :: forall obj o i x y . LayAppend o i y obj x -> Wit obj (Lay o i (x :++: y))
getLayAppend (LayAppend w) = w
layAppend :: forall hom obj o i x y.
  (Premonoidal hom obj o i,
   FiniteList obj x,
   FiniteList obj y)
  => Wit obj (Lay o i (x :++: y))
layAppend = getLayAppend @obj @o @i @x @y $ induction basecase generalcase
  where
    basecase :: LayAppend o i y obj '[]
    basecase = LayAppend (layObj @hom @obj @o @i @y)
    generalcase :: (obj a, FiniteList obj z) => LayAppend o i y obj z -> LayAppend o i y obj (a ': z)
    generalcase (LayAppend Wit) = (LayAppend Wit)


-- laytensorForward :: Premonoidal hom obj o i => hom (Lay o i x `o` Lay o i y) (Lay o i (x :++: y))
-- laytensorBackward :: Premonoidal hom obj o i => hom (Lay o i (x :++: y)) (Lay o i x `o` Lay o i y)

-- Construction 1. A morphism from the tensor of two lists to the concatenation.
data InductionHypothesisF hom o i y (obj :: * -> Constraint) x where
  InductionHypothesisF ::
    hom (Lay o i x `o` Lay o i y) (Lay o i (x :++: y))
    -> InductionHypothesisF hom o i y obj x
getInductionHypothesisF :: InductionHypothesisF hom o i y obj x -> hom (Lay o i x `o` Lay o i y) (Lay o i (x :++: y))
getInductionHypothesisF (InductionHypothesisF i) = i

laytensorForward :: forall hom obj o i x y . (Premonoidal hom obj o i, FiniteList obj x, FiniteList obj y)
  => hom (Lay o i x `o` Lay o i y) (Lay o i (x :++: y))
laytensorForward = getInductionHypothesisF byInduction
    where
     byInduction :: (Premonoidal hom obj o i, FiniteList obj x) => InductionHypothesisF hom o i y obj x
     byInduction = induction
        (basecase (layObj @hom @obj @o @i @y))
        (generalcase (layObj @hom @obj @o @i @y)) -- (basecase (witness @hom)) (generalcase (witness @hom))
      where
        basecase :: Wit obj (Lay o i y) -> InductionHypothesisF hom o i y obj '[]
        basecase Wit = InductionHypothesisF lambda

        generalcase :: forall hom obj o i a z . (FiniteList obj y, Premonoidal hom obj o i, FiniteList obj z, obj a) =>
              Wit obj (Lay o i y)
          -> InductionHypothesisF hom o i y obj z
          -> InductionHypothesisF hom o i y obj (a ': z)
        generalcase Wit = auxiliary (layObj @hom @obj @o @i @z) (layAppend @hom @obj @o @i @z @y) Wit
          where
            auxiliary :: forall hom obj o i a z . (Premonoidal hom obj o i, FiniteList obj z, obj a) =>
                  Wit obj (Lay o i z)
              -> Wit obj (Lay o i (z :++: y))
              -> Wit obj (Lay o i y)
              -> InductionHypothesisF hom o i y obj z
              -> InductionHypothesisF hom o i y obj (a ': z)
            auxiliary Wit Wit Wit (InductionHypothesisF inductionHypothesis) =
              InductionHypothesisF
                ((>)  alphainv
                 |<|  tensor (identity @hom @obj @a) inductionHypothesis)

-- Construction 2. A morphism to the tensor of two lists from the concatenation.
data InductionHypothesisB hom o i y (obj :: * -> Constraint) x where
  InductionHypothesisB ::
    hom (Lay o i (x :++: y)) (Lay o i x `o` Lay o i y)
    -> InductionHypothesisB hom o i y obj x
getInductionHypothesisB :: InductionHypothesisB hom o i y obj x -> hom (Lay o i (x :++: y)) (Lay o i x `o` Lay o i y)
getInductionHypothesisB (InductionHypothesisB i) = i

laytensorBackward :: forall hom obj o i x y . (Premonoidal hom obj o i, FiniteList obj x, FiniteList obj y)
  => hom (Lay o i (x :++: y)) (Lay o i x `o` Lay o i y)
laytensorBackward = getInductionHypothesisB byInduction
    where
     byInduction :: (Premonoidal hom obj o i, FiniteList obj x) => InductionHypothesisB hom o i y obj x
     byInduction = induction
        (basecase (layObj @hom @obj @o @i @y))
        (generalcase (layObj @hom @obj @o @i @y))
      where
        basecase :: Wit obj (Lay o i y) -> InductionHypothesisB hom o i y obj '[]
        basecase Wit = InductionHypothesisB lambdainv

        generalcase :: forall hom obj o i a z . (FiniteList obj y, Premonoidal hom obj o i, FiniteList obj z, obj a) =>
              Wit obj (Lay o i y)
          -> InductionHypothesisB hom o i y obj z
          -> InductionHypothesisB hom o i y obj (a ': z)
        generalcase Wit = auxiliary (layObj @hom @obj @o @i @z) (layAppend @hom @obj @o @i @z @y) Wit
          where
            auxiliary :: forall hom obj o i a z . (Premonoidal hom obj o i, FiniteList obj z, obj a) =>
                  Wit obj (Lay o i z)
              -> Wit obj (Lay o i (z :++: y))
              -> Wit obj (Lay o i y)
              -> InductionHypothesisB hom o i y obj z
              -> InductionHypothesisB hom o i y obj (a ': z)
            auxiliary Wit Wit Wit (InductionHypothesisB inductionHypothesis) =
              InductionHypothesisB
                ( tensor (identity @hom @obj @a) inductionHypothesis |<|  alpha)


-- Definition of polymorphisms.
data Morphism hom (o :: * -> * -> *) (i :: *) (x :: [*]) (y :: [*]) :: * where
  Morphism :: hom (Lay o i x) (Lay o i y) -> Morphism hom o i x y
getMorphism :: Morphism hom o i x y -> hom (Lay o i x) (Lay o i y)
getMorphism (Morphism f) = f

swap :: (SymmetricPremonoidal hom obj o i, obj x, obj y)
  => Morphism hom o i '[x,y] '[y,x]
swap = Morphism
  $ composition alpha
  $ composition (tensor sigma identity)
  $ alphainv

wire :: (Premonoidal hom obj o i, obj x) => Morphism hom o i '[x] '[x]
wire = Morphism identity
