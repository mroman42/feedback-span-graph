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

module Function where

import Category
import Premonoidal
import Morphism
import Any

-- Category of sets and functions.
newtype Function a b = Function { getFunction :: a -> b }
instance Category Function Any where
  identity = Function id
  composition (Function f) (Function g) = Function (g . f)

-- The cartesian monoidal category of sets and functions.
instance Premonoidal Function Any (,) () where
  alpha = Function (\(x,(y,z)) -> ((x,y),z))
  alphainv = Function (\((x,y),z) -> (x,(y,z)))
  rho = Function (\(x,_) -> x)
  rhoinv = Function (\x -> (x,()))
  lambda = Function (\(_,x) -> x)
  lambdainv = Function (\x -> ((),x))
  tensor (Function f) (Function g) = Function (\(x,y) -> (f x, g y))

instance SymmetricPremonoidal Function Any (,) () where
  sigma = Function (\(a,b) -> (b,a))

-- Unbiased morphisms.
type Sets = Morphism Function (,) ()
