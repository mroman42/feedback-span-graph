-- | Premonoidal.hs

-- Premonoidal categories in Haskell. A premonoidal category (* -> * -> *) is a
-- category together with coherence morphisms and a tensor operation, which does
-- not need to be functorial.
--
-- The objects of the premonoidal category are given by a constraint that can be
-- satisfied by some Haskell types.

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module Premonoidal where

import Control.Monad
import Data.Finitary
import Any

-- | Definition of a category enriched over the language. The sets of objects
-- are represented by constraints, making it slightly more general than
-- Data.Category.
class Category hom obj | hom -> obj where
  identity :: (obj x) => hom x x
  composition :: (obj x, obj y, obj z) => hom x y -> hom y z -> hom x z

-- | Composition operator.
(|<|) :: (Category hom obj, obj x, obj y, obj z) => hom x y -> hom y z -> hom x z
(|<|) f g = composition f g




-- | Premonoidal category, definition.
class (Category hom obj, obj i, (forall x y . (obj x , obj y) => obj (x `o` y) ))
      => Premonoidal hom obj o i | hom -> obj o i where
  alpha :: (obj x, obj y, obj z) => hom (x `o` (y `o` z)) ((x `o` y) `o` z)
  alphainv  :: (obj x, obj y, obj z) => hom ((x `o` y) `o` z) (x `o` (y `o` z))
  rho    :: (obj x) => hom (x `o` i) x
  rhoinv :: (obj x) => hom x (x `o` i)
  lambda       :: (obj x) => hom (i `o` x) x
  lambdainv    :: (obj x) => hom x (i `o` x)
  tensor :: (obj x1, obj y1, obj x2 , obj y2) => hom x1 y1 -> hom x2 y2 -> hom (x1 `o` x2) (y1 `o` y2)


-- | A symmetric premonoidal category is a premonoidal category with an extra
-- morphism A o B -> B o A called the "braiding". This braiding is assumed to be
-- invertible with its component in (A,B) being the inverse of the component in
-- (B,A).
class (Premonoidal hom obj o i) => SymmetricPremonoidal hom obj o i where
  sigma :: (obj x , obj y) => hom (x `o` y) (y `o` x)
