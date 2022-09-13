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

module Kleisli where

import Control.Monad
import Category
import Premonoidal
import Function
import Any


-- Kleisli category of a monad.
newtype Kleisli m a b = Kleisli { getKleisli :: (a -> m b) }
instance (Monad m) => Category (Kleisli m) Any where
  identity = Kleisli return
  composition (Kleisli f) (Kleisli g) = Kleisli (join . fmap g . f)

-- The premonoidal kleisli category of a monad.
kleisliLift :: (Monad m) => Function a b -> Kleisli m a b
kleisliLift (Function f) = Kleisli (return . f)
instance (Monad m) => Premonoidal (Kleisli m) Any (,) () where
  alpha = kleisliLift alpha
  alphainv = kleisliLift alphainv
  rho = kleisliLift rho
  rhoinv = kleisliLift rhoinv
  lambda = kleisliLift lambda
  lambdainv = kleisliLift lambdainv
  tensor (Kleisli f) (Kleisli g) = Kleisli $ \(x, y) -> do
    a <- f x
    b <- g y
    return (a , b)
