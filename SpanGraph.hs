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


module SpanGraph where

import Prelude hiding ((>))
import Category
import Premonoidal
import Morphism
import Data.Finitary
import Data.Void
import Data.Finite.Internal
import Data.Set
import State
import Function
import Any
import Span
import GHC.Generics (Generic)
import BellsAndWhistles

-- SpanGraph
type SpanGraph' = State Span' Finitary (,) ()
type SpanGraph = Morphism SpanGraph' (,) ()


norLatchSpanGraph :: SpanGraph '[Bool,Bool] '[Bool,Bool]
norLatchSpanGraph =
  ((>))   wire |*| codiscard |*| codiscard |*| wire
   |>|    wire |*|   copy    |*|   copy    |*| wire
   |>|       nor     |*| swap  |*|       nor
   |>|    copy |*|   wire    |*|   wire    |*| copy
   |>|  wire |*| delay |*| wire |*| wire |*| delay |*| wire
   |>|    wire |*| cocopy    |*|   cocopy |*|  wire
   |>|    wire |*| discard   |*|  discard |*|  wire




draw :: SpanGraph '[Bool,Bool] '[Bool,Bool] -> String -> IO ()
draw (Morphism (State (Span' f))) caption = do
  putStr $ unlines
     [ "digraph norlatch {"
     , "pad = 0.2;"
     , "nodesep = 0.5;"
     , "layout=circo;"
     , "graph [fontsize=16 label=" ++ show caption ++ " rankdir=BT]"
     , "node [fontname=Iosevka fontsize=30 margin=0.1 shape=square]"
     , "edge [arrowhead=open fontname=Iosevka fontsize=14 style=dashed tailport=c]"
     , "forcelabels=true;"
     , "overlap=false;" ]
  sequence $ Prelude.map (\((s,(x,(y,_))),(t,(a,(b,()))))->
    drawTransition (s,x,y) (t,a,b)) f
  putStrLn "}"
  return ()
 where
   drawSignal :: (Bool,Bool) -> String
   drawSignal (True,False) = "Set"
   drawSignal (False,True) = "Reset"
   drawSignal (False,False) = "Idle"
   drawSignal (True,True) = "Unspec"

   drawTransition :: (Finitary a) => (a,Bool,Bool) -> (a,Bool,Bool) -> IO ()
   drawTransition (s,x,y) (t,a,b) = putStr $ unlines
       [ input ++ " -> " ++ output ++ " [xlabel=" ++ signal ++ "];" ]
     where
       signal = drawSignal (x,y)
       input = show $ getFinite $ toFinite s
       output = show $ getFinite $ toFinite t
       transition = concat [signal, "I", input, "O", output]
