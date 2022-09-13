-- | Coherence for Monoidal categories

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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses   #-}


module Skew where

import Categories
import Unsafe.Coerce

class Monoidal pi u | pi -> u where
  lambdaR :: pi u x -> x
  lambdaRi :: x -> pi u x
  lambdaL :: pi x u -> x
  lambdaLi :: x -> pi x u
  assocR :: pi x (pi y z) -> pi (pi x y) z
  assocL :: pi (pi x y) z -> pi x (pi y z)
  tensor :: (x1 -> y1) -> (x2 -> y2) -> pi x1 x2 -> pi y1 y2

instance Monoidal (,) () where
  lambdaR ((),x) = x
  lambdaRi (x) = ((),x)
  lambdaL (x,()) = x
  lambdaLi x = (x,())
  assocR (a,(b,c)) = ((a,b),c)
  assocL ((a,b),c) = (a,(b,c))
  tensor f g (x,y) = (f x, g y)

-- type family Skew (o :: * -> * -> *) (i :: *) (x :: *) :: *
-- type instance Skew o i ((a1 `o` a2) `o` b) = Skew o i (a1 `o` (a2 `o` b))
-- type instance Skew o i (i `o` b) = Skew o i b
-- type instance Skew o i (a `o` b) = a `o` Skew o i b
-- type instance Skew o i a = a `o` i


-- class Skewing (o :: * -> * -> *) (i :: *) (x :: *) where
--   type Skew o i x :: *
--   skew :: x -> Skew o i x
--   unskew :: Skew o i x -> x

-- instance {-# OVERLAPS #-} (Monoidal o i, Skewing o i (a1 `o` (a2 `o` b))) => Skewing o i ((a1 `o` a2) `o` b) where
--   type Skew o i ((a1 `o` a2) `o` b) = Skew o i (a1 `o` (a2 `o` b))
--   skew = undefined
--   unskew = undefined

-- instance (Monoidal o i, Skewing o i x) => Skewing o i (i `o` x) where
--   type Skew o i (i `o` x) = Skew o i x
--   skew = skew @o @i . lambdaR
--   unskew = undefined




type family Skew (o :: * -> * -> *) (i :: *) (x :: *) :: * where
  Skew o i ((a1 `o` a2) `o` b) = Skew o i (a1 `o` (a2 `o` b))
  Skew o i (i `o` b) = Skew o i b
  Skew o i (a `o` b) = a `o` Skew o i b
  Skew o i a = a `o` i

-- coherence :: (MonoidalCategory obj hom o i, obj a) => a -> Skew o i a
-- coherence = _


class Skewing (o :: * -> * -> *) (i :: *) (x :: *) where
  skew :: x -> Skew o i x
  unskew :: Skew o i x -> x

instance (Monoidal o i, Skewing o i (a1 `o` (a2 `o` b))) => Skewing o i ((a1 `o` a2) `o` b) where
  skew = skew @o @i . assocL @o @i
  unskew = assocR @o @i . unskew @o @i

instance (Monoidal o i, Skewing o i b) => Skewing o i (i `o` b) where
  skew = unsafeCoerce . skew @o @i . lambdaR @o @i
  unskew = lambdaRi . unskew @o @i . unsafeCoerce

instance (Monoidal o i, Skewing o i b) => Skewing o i (a `o` b) where
  skew = unsafeCoerce . tensor @o @i id (skew @o @i)
  unskew = tensor @o @i id (unskew @o @i) . unsafeCoerce

instance (Monoidal o i) => Skewing o i a where
  skew = unsafeCoerce . lambdaLi @o @i
  unskew = lambdaL @o @i . unsafeCoerce


coherence :: forall o i a b . (Monoidal o i, Skew o i a ~ Skew o i b) => (a -> b)
coherence = unskew @o @i . skew @o @i






-------- TESTS
x :: Skew (,) () (((),Int),(Int,Int))
x = undefined

doing :: (Int,((Int,(Int,())),Int)) -> (Int,(Int,(Int,(Int,()))))
doing = skew @(,) @()

-- coherence = unskew . skew


-- (<***>) :: forall o i x1 y1 x2 y2 . Monoidal o i => (x1 -> y1) -> (x2 -> y2) -> Skew o i (o x1 x2) -> Skew o i (o y1 y2)
-- (<***>) = tensor @o @i

test :: () -> Int
test =
    plus                                                    .unskew @(,) @().skew @(,) @().
    ((\()-> 2 :: Int) <***> (\()-> 3 :: Int))       .unskew @(,) @().skew @(,) @()
  where
    (<***>) = tensor @(,) @()
    plus :: (Int,Int) -> Int
    plus = uncurry (+)


test2 :: (Int,(Int,Int)) -> Int
test2 =
    plus                       .unskew @(,) @().skew @(,) @().
    (plus <*$*> (id @ Int))    .unskew @(,) @().skew @(,) @()
  where
    infixl 8 <*$*>
    (<*$*>) = tensor @(,) @()
    plus :: (Int,Int) -> Int
    plus = uncurry (+)


op :: () -> ((),())
op = unskew @(,) @() . skew @(,) @()

g :: (Int,(Int,Int)) -> (Int,(Int,(Int,())))
g = skew @(,) @()
h :: (Int,(Int,(Int,()))) -> ((Int,Int),Int)
h = unskew @(,) @()
j :: (Int,(Int,Int)) -> ((Int,Int),Int)
j = unskew @(,) @() . skew @(,) @()
k :: ((Int,()),(Int,Int)) -> ((Int,(Int,())),Int)
k = unskew @(,) @() . skew @(,) @()

f :: (Int,(Int,Int)) -> ((Int,Int),Int)
f = coherence @(,) @()

o :: ((Int,()),(Int,Int)) -> ((Int,(Int,())),Int)
o = coherence @(,) @()

t :: ((Int,Int),Int)
t = f (2,(3,4))
