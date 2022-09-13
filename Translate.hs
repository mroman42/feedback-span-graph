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

module Translate where

import Data.Constraint
import Category
import Premonoidal
import Prelude hiding ((>))
import Morphism


fromABB ::  (Premonoidal hom obj o i, obj x, obj y, obj z)
  => hom x (y `o` z)
  -> Morphism hom o i '[x] '[y,z]
fromABB f = Morphism
  $ composition (tensor f identity)
  $ alphainv


fromAAB ::  (Premonoidal hom obj o i, obj x, obj y, obj z)
  => hom (x `o` y) z
  -> Morphism hom o i '[x,y] '[z]
fromAAB f = Morphism
  $ composition alpha (tensor f identity)


fromAABB :: (Premonoidal hom obj o i, obj m, obj x, obj p, obj y)
  => hom (m `o` x) (p `o` y)
  -> Morphism hom o i '[m,x] '[p,y]
fromAABB f = Morphism
  $ composition alpha
  $ composition rho
  $ composition f
  $ composition rhoinv
  $ alphainv

toAAoAABBoBB :: (Premonoidal hom obj o i,
                 obj s1, obj s2, obj t1, obj t2,
                 obj x1, obj x2, obj y1, obj y2)
  => Morphism hom o i '[s1, s2, x1, x2] '[t1, t2, y1, y2]
  -> hom (o (o s1 s2) (o x1 x2)) (o (o t1 t2) (o y1 y2))
toAAoAABBoBB (Morphism f) =
    composition (tensor identity rhoinv)
    $ composition (tensor identity alphainv)
    $ composition alphainv
    $ composition f
    $ composition alpha
    $ composition (tensor identity alpha)
    $ tensor identity rho

toAAoABBoB :: (Premonoidal hom obj o i, obj m, obj n, obj x, obj p, obj q, obj z)
  => Morphism hom o i '[m,n,x] '[p,q,z]
  -> hom ((m `o` n) `o` x) ((p `o` q) `o` z)
toAAoABBoB (Morphism f) =
    composition alphainv
  $ composition (tensor identity rhoinv)
  $ composition (tensor identity alphainv)
  $ composition f
  $ composition (tensor identity alpha)
  $ composition (tensor identity rho)
  $ alpha
