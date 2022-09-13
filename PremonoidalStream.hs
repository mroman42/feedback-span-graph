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

module PremonoidalStream where

import Prelude hiding ((>))
import Data.Functor.Identity
import Data.List

import System.Random
import System.IO.Unsafe
import Category
import Premonoidal
import Morphism
import Translate
import Control.Arrow
import MonoidalStreams
import Any

instance
    Arrow c =>
    Category (Stream c) Any
  where
     identity = idS
     composition = comp

instance
     Arrow c =>
     Premonoidal (Stream c) Any (,) ()
  where
    alpha = assoc
    alphainv = associnv
    rho = runit
    rhoinv = runitinv
    lambda = lunit
    lambdainv = lunitinv
    tensor = MonoidalStreams.tensor

instance
    Arrow c =>
    SymmetricPremonoidal (Stream c) Any (,) ()
  where
     sigma = MonoidalStreams.sigma



-- -- Fixpoint equation for monoidal streams. Figure 5.
-- type Stream hom obj o i = StreamWithMemory hom obj o i i

-- data StreamWithMemory hom obj o i n x y where
--   StreamWithMemory :: (Premonoidal hom obj o i, obj n, obj m, obj x, obj y) =>
--      hom (n `o` x) (m `o` y)
--     -> StreamWithMemory hom obj o i m x y
--     -> StreamWithMemory hom obj o i n x y

-- lact :: (SymmetricPremonoidal hom obj o i, obj n, obj m, obj x, obj y) =>
--   hom m n ->
--   StreamWithMemory hom obj o i n x y ->
--   StreamWithMemory hom obj o i m x y
-- lact f (StreamWithMemory now later) = StreamWithMemory (composition (tensor f identity) now) later

-- idS :: (SymmetricPremonoidal hom obj o i, obj x) =>
--   StreamWithMemory hom obj o i i x x
-- idS = StreamWithMemory identity idS

-- compS :: (SymmetricPremonoidal hom obj o i, obj n, obj m, obj x, obj y, obj z) =>
--   StreamWithMemory hom obj o i m x y ->
--   StreamWithMemory hom obj o i n y z ->
--   StreamWithMemory hom obj o i (m `o` n) x z
-- compS
--   (StreamWithMemory fnow flater)
--   (StreamWithMemory gnow glater) =
--   StreamWithMemory (sequentialComposition fnow gnow) (compS flater glater)
--  where

--    -- Definition 5.2.
--    -- Sequential composition, "now".
--    sequentialComposition :: forall hom obj o i n m x y z p q .
--      (SymmetricPremonoidal hom obj o i, obj n, obj m, obj x, obj y, obj z, obj p, obj q)
--      => hom (m `o` x) (p `o` y)
--      -> hom (n `o` y) (q `o` z)
--      -> hom ((m `o` n) `o` x) ((p `o` q) `o` z)
--    sequentialComposition fs gs =
--      let (f,g) = (fromAABB fs, fromAABB gs) in toAAoABBoB $

--       ((>))      swap  |*| wire
--        |>|   wire |*|    f
--        |>|       swap  |*| wire
--        |>|   wire |*|    g

-- comp :: (SymmetricPremonoidal hom obj o i, obj x, obj y, obj z)
--   => Stream hom obj o i x y -> Stream hom obj o i y z -> Stream hom obj o i x z
-- comp f g = lact lambdainv (compS f g)

-- instance
--     SymmetricPremonoidal hom obj o i =>
--     Category (Stream hom obj o i) obj
--   where
--     identity = idS
--     composition = comp

-- liftS ::
--    (SymmetricPremonoidal hom obj o i, obj x, obj y) =>
--    hom x y ->
--    Stream hom obj o i x y
-- liftS f = StreamWithMemory (tensor identity f) (liftS f)


-- instance
--     SymmetricPremonoidal hom obj o i =>
--     Premonoidal (Stream hom obj o i) obj o i
--   where
--     alpha = liftS alpha
--     alphainv = liftS alphainv
--     rho = liftS rho
--     rhoinv = liftS rhoinv
--     lambda = liftS lambda
--     lambdainv = liftS lambdainv
--     tensor = undefined

-- instance
--     SymmetricPremonoidal hom obj o i =>
--     SymmetricPremonoidal (Stream hom obj o i) obj o i
--   where
--     sigma = liftS sigma


-- tensorS :: (Arrow c) =>
--   StreamWithMemory c p x y ->
--   StreamWithMemory c p' x' y' ->
--   StreamWithMemory c (p , p') (x,x') (y,y')
-- tensorS
--   (StreamWithMemory fnow flater)
--   (StreamWithMemory gnow glater) =
--   StreamWithMemory (parallelCompTosition fnow gnow) (tensorS flater glater)
--  where

--    -- Definition 5.3. Parallel compTosition.
--    parallelCompTosition :: Arrow c
--      => c (m,x) (p,z)
--      -> c (n,y) (q,w)
--      -> c ((m,n),(x,y)) ((p,q),(z,w))
--    parallelCompTosition f g =
--          associnv                   >>> id *** assoc
--      >>> (id *** (sigma *** id))  >>> id *** associnv >>> assoc
--      >>> (f *** g)                >>> associnv            >>> id *** assoc
--      >>> (id *** (sigma *** id))  >>> id *** associnv >>> assoc

-- tensor :: Arrow c => Stream c x y -> Stream c x' y' -> Stream c (x,x') (y,y')
-- tensor f g = lact lunitinv (tensorS f g)
