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



module BellsAndWhistles where


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
import Translate

class (Premonoidal hom obj o i) => Freyd hom obj o i where
  lift :: (obj a, obj b) => (a -> b) -> hom a b
class (Premonoidal hom obj o i) => Cofreyd hom obj o i where
  colift :: (obj a, obj b) => (b -> a) -> hom a b

copy :: (Freyd hom obj (,) (), obj x) => Morphism hom (,) () '[x] '[x,x]
copy = Morphism $ lift (\(x,()) -> (x,(x,())))

discard :: (Freyd hom obj (,) (), obj x) => Morphism hom (,) () '[x] '[]
discard = Morphism $ lift (\(x,()) -> ())

cocopy :: (Cofreyd hom obj (,) i, obj x) => Morphism hom (,) i '[x,x] '[x]
cocopy = fromAAB (colift (\x -> (x,x)))

codiscard :: (Cofreyd hom obj (,) (), obj x) => Morphism hom (,) () '[] '[x]
codiscard = Morphism $ colift (\(x,()) -> ())


nor :: (Freyd hom obj (,) (), obj Bool) => Morphism hom (,) () '[Bool,Bool] '[Bool]
nor = Morphism $ lift $ \(a,(b,())) -> (not (or [a,b]),())
