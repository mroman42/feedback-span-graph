-- |
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

module PremonoidalStreamExample where

import Prelude hiding ((>), id)
import Data.Functor.Identity
import Data.List
import Control.Category
import System.Random
import System.IO.Unsafe
import Category
import Premonoidal
import Morphism
import Translate
import Control.Arrow
import MonoidalStreams
import Any
import PremonoidalStream
import Unsafe.Coerce

fibonacci' :: Stream (Kleisli Identity) () (Int,())
fibonacci' = fbk $ getMorphism $
  ((>)) copy'
   |>|  delay' (k1' |*| wait') |*| wire
   |>|  delay' fby' |*| wire
   |>|  plus'
   |>|  k0' |*| wire
   |>|  fby'
   |>|  copy'

copy' :: (Monad t) => Morphism (Stream (Kleisli t)) (,) () '[a] '[a,a]
copy' = Morphism $ lift (Kleisli $ \(a,()) -> pure (a,(a,())))
fby' :: (Monad t) => Morphism (Stream (Kleisli t)) (,) ()  '[a,a] '[a]
fby' = Morphism $ StreamWithMemory (Kleisli $ \((),(x,(y,()))) -> pure ((),(x,()))) (lift (arr snd))
k0',k1',k2' :: (Arrow c) => Morphism (Stream c) (,) ()  '[] '[Int]
k0' = Morphism $ lift $ arr (\() -> (0,()))
k1' = Morphism $ lift $ arr (\() -> (1,()))
k2' = Morphism $ lift $ arr (\() -> (2,()))

delay' :: (Arrow c) =>
  Morphism (Stream c) (,) () x y
  ->   Morphism (Stream c) (,) () x y
delay' (Morphism f) = Morphism $ StreamWithMemory (id *** (unsafeCoerce 0)) f

plus' :: (Arrow c) => Morphism (Stream c) (,) () '[Int,Int] '[Int]
plus' = Morphism $ lift $ arr (\(a,(b,())) -> (a + b,()))

wait' :: (Arrow c) => Morphism (Stream c) (,) () '[a] '[a]
wait' = Morphism $ fbk sigmaS





fibonacci :: Stream (Kleisli Identity) () Int
fibonacci = fbk                             $ runitS
  >>> copy                                >>> lunitinvS *** id
  >>> delay (k1 *** wait) *** id
  >>> delay fby *** id
  >>> plus                                >>> lunitinvS
  >>> k0 *** id
  >>> fby
  >>> copy
