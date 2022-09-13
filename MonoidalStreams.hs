-- | Monoidal Streams for Dataflow Programming.

-- Anonymous.

-- The following code implements the category of monoidal streams over a
-- monoidal category with an identity-on-objects functor from the
-- (pseudo)category of Haskell types.

-- During the manuscript, we have needed to perform some computations: for
-- instance, to see that according to our definitions the Fibonacci morphism we
-- describe really computes the Fibonacci sequence. Computations of this kind
-- are difficult and tedious to write and to justify, and the reader may find
-- difficult to reproduce them. Instead of explicitly writing these
-- computations, we implement them and we provide the necessary code so that the
-- reader can verify the result from the computation.

-- Morphisms in a monoidal category are written in "Arrow" notation, using (>>>)
-- for sequential composition and (***) for parallel composition. Coherence
-- morphisms need to be written explicitly, we usually write them at the side of
-- the diagram.

{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances        #-}

module MonoidalStreams where

import Prelude hiding (id)
import Data.Functor.Identity
import Data.List
import Control.Category
import Control.Arrow
import System.Random
import System.IO.Unsafe
import Unsafe.Coerce

-- Fixpoint equation for monoidal streams. Figure 5.
type Stream c = StreamWithMemory c ()

data StreamWithMemory c n x y where
  StreamWithMemory :: (Arrow c) =>
     c (n , x) (m , y)
    -> StreamWithMemory c m x y
    -> StreamWithMemory c n x y


--------------
-- EXAMPLES --
--------------
fibonacci :: Stream (Kleisli Identity) () Int
fibonacci = fbk                             $ runitS
  >>> copy                                >>> lunitinvS *** id
  >>> delay (k1 *** wait) *** id
  >>> delay fby *** id
  >>> plus                                >>> lunitinvS
  >>> k0 *** id
  >>> fby
  >>> copy

walk :: Stream (Kleisli IO) (()) (Int)
walk = fbk
    $ (id *** unif)
  >>> plus                                >>> lunitinvS
  >>> k0 *** id
  >>> fby
  >>> copy
 where
   unif :: Stream (Kleisli IO) () Int
   unif = lift $ Kleisli (\() -> do
      boolean <- randomIO
      return $ if boolean then 1 else -1)

type Urn = [Int]


ehrenfest :: Stream (Kleisli IO) (()) (Urn,Urn)
ehrenfest = fbk                          $ runitS>>> lunitinv *** lunitinv
  >>> (full *** idS) *** (empty *** idS)    >>> runitinv
  >>> (fby *** fby) *** unif
  >>> idS *** copy                          >>> associnv >>> idS *** assoc
  >>> idS *** (sigma *** idS)               >>> idS *** associnv >>> assoc
  >>> move *** move
  >>> copy *** copy                          >>> associnv >>> idS *** assoc
  >>> idS *** (sigma *** idS)               >>> idS *** associnv >>> assoc
  where
    unif :: Stream (Kleisli IO) () Int
    unif = lift $ Kleisli (\() -> randomRIO (1,4))

    empty :: Stream (Kleisli IO) () Urn
    empty = lift $ arr (\() -> [])

    full :: Stream (Kleisli IO) () Urn
    full = lift $ arr (\() -> [1,2,3,4])

    move :: Stream (Kleisli IO) (Urn, Int) Urn
    move = lift $ arr (\(u,i) ->
      if elem i u
        then (delete i u)
        else (insert i u))

--- take 10 <$> run fibonacci
--- take 10 <$> run walk
--- take 10 <$> run ehrenfest



---------------------------
-- THE FEEDBACK CATEGORY --
---------------------------

compS :: (Arrow c) =>
  StreamWithMemory c m x y ->
  StreamWithMemory c n y z ->
  StreamWithMemory c (m , n) x z
compS
  (StreamWithMemory fnow flater)
  (StreamWithMemory gnow glater) =
  StreamWithMemory (sequentialComposition fnow gnow) (compS flater glater)
 where

   -- Definition 5.2.
   -- Sequential composition, "now".
   sequentialComposition :: Arrow c
     => c (m , x) (p , y)
     -> c (n , y) (q , z)
     -> c ((m,n),x) ((p,q),z)
   sequentialComposition f g =
        sigma  *** id    >>> associnv
    >>> id *** f       >>> assoc
    >>> sigma  *** id  >>> associnv
    >>> id *** g       >>> assoc

comp :: (Arrow c) => Stream c x y -> Stream c y z -> Stream c x z
comp f g = lact lunitinv (compS f g)



tensorS :: (Arrow c) =>
  StreamWithMemory c p x y ->
  StreamWithMemory c p' x' y' ->
  StreamWithMemory c (p , p') (x,x') (y,y')
tensorS
  (StreamWithMemory fnow flater)
  (StreamWithMemory gnow glater) =
  StreamWithMemory (parallelCompTosition fnow gnow) (tensorS flater glater)
 where

   -- Definition 5.3. Parallel compTosition.
   parallelCompTosition :: Arrow c
     => c (m,x) (p,z)
     -> c (n,y) (q,w)
     -> c ((m,n),(x,y)) ((p,q),(z,w))
   parallelCompTosition f g =
         associnv                   >>> id *** assoc
     >>> (id *** (sigma *** id))  >>> id *** associnv >>> assoc
     >>> (f *** g)                >>> associnv            >>> id *** assoc
     >>> (id *** (sigma *** id))  >>> id *** associnv >>> assoc

tensor :: Arrow c => Stream c x y -> Stream c x' y' -> Stream c (x,x') (y,y')
tensor f g = lact lunitinv (tensorS f g)


lact :: (Arrow c) => c n m -> StreamWithMemory c m x y -> StreamWithMemory c n x y
lact f (StreamWithMemory now later) = StreamWithMemory ((f *** id) >>> now) later


fbkS :: (Arrow c) =>
  StreamWithMemory c m (s,x) (s,y) ->
  StreamWithMemory c (m, s) x y
fbkS (StreamWithMemory now later) =
  StreamWithMemory (nowFeedback now) (fbkS later)
 where

   -- Definition 5.7. Feedback operation.
   nowFeedback :: (Arrow c) => c (m,(s,x)) (n,(t,y)) -> c ((m,s),x) ((n,t),y)
   nowFeedback f = associnv >>> f >>> assoc

fbk :: (Arrow c) => Stream c (s,x) (s,y) -> Stream c x y
fbk t = lact (arr (\() -> ((),unsafeCoerce 0))) (fbkS t)

idS :: (Arrow c) => Stream c x x
idS = StreamWithMemory (id) idS


lift :: (Arrow c) => c x y -> Stream c x y
lift f = StreamWithMemory (id *** f) (lift f)

liftarr :: (Arrow c) => (x -> y) -> Stream c x y
liftarr s = lift $ arr s

instance (Arrow c) => Category (Stream c) where
  id = idS
  (.) f g = comp g f

instance (Arrow c) => Arrow (Stream c) where
  arr = liftarr
  (***) = tensor

instance (Arrow c) => ArrowLoop (Stream c) where
  loop f = fbk $ sigma >>> f >>> sigma


delay :: (Arrow c) => Stream c x y -> Stream c x y
delay f = StreamWithMemory (id *** undefined)  f




------------
-- ARROWS --
------------
assoc :: Arrow c => c (x,(y,z)) ((x,y),z)
assoc = arr $ \(x,(y,z)) -> ((x,y),z)
assocS :: Arrow c => Stream c (x,(y,z)) ((x,y),z)
assocS = lift assoc

associnv :: Arrow c => c ((x,y),z) (x,(y,z))
associnv = arr $ \((x,y),z) -> (x,(y,z))
associnvS :: Arrow c => Stream c ((x,y),z) (x,(y,z))
associnvS = lift $ associnv

lunit :: Arrow c => c ((),a) a
lunit = arr $ \((),a) -> a
lunitS :: Arrow c => Stream c ((),a) a
lunitS = lift $ lunit

lunitinv :: Arrow c => c a ((),a)
lunitinv = arr $ \a -> ((),a)
lunitinvS :: Arrow c => Stream c a ((),a)
lunitinvS = lift $ lunitinv

runit :: Arrow c => c (a,()) a
runit = arr $ \(a,()) -> a
runitS :: Arrow c => Stream c (a,()) a
runitS = lift $ runit

runitinv :: Arrow c => c a (a,())
runitinv = arr $ \a -> (a,())
runitinvS :: Arrow c => Stream c a (a,())
runitinvS = lift $ runitinv


sigma :: Arrow c => c (x,y) (y,x)
sigma = arr $ \(x,y) -> (y,x)

sigmaS :: Arrow c => Stream c (x,y) (y,x)
sigmaS = lift $ sigma


----------------
-- GENERATORS --
----------------
fby :: (Monad t) => Stream (Kleisli t) (a , a) a
fby = StreamWithMemory (Kleisli $ \((),(x,y)) -> pure ((),x)) (lift (arr snd))

copy :: (Monad t) => Stream (Kleisli t) a (a,a)
copy = lift (Kleisli $ \a -> pure (a,a))


k0,k1,k2 :: (Arrow c) => Stream c () Int
k0 = lift $ arr (\() -> 0)
k1 = lift $ arr (\() -> 1)
k2 = lift $ arr (\() -> 2)

plus :: (Arrow c) => Stream c (Int,Int) Int
plus = lift $ arr (\(a,b) -> a + b)

wait :: (Arrow c) => Stream c a a
wait = fbk sigmaS

------------
-- SYSTEM --
------------

class (Monad m) => IOMonad m where unsafeRun :: m a -> m a
instance IOMonad IO where unsafeRun = unsafeInterleaveIO
instance IOMonad Identity where unsafeRun = id


runUnsafeWithMemory :: (IOMonad t) => m -> StreamWithMemory (Kleisli t) m a b -> [a] -> t [b]
runUnsafeWithMemory m (StreamWithMemory (Kleisli now) later) (a:as) = do
  (n , b)<- now (m , a)
  l <- unsafeRun $ runUnsafeWithMemory n later as
  pure (b : l)

runUnsafe :: (IOMonad t) => Stream (Kleisli t) a b -> [a] -> t [b]
runUnsafe = runUnsafeWithMemory ()

run :: (IOMonad t) => Stream (Kleisli t) () a -> t [a]
run s = runUnsafe s (repeat ())

------------------------------------------

main :: IO ()
main = return ()
