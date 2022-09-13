-- |

module Quantale where

import Data.Finitary
import Data.Void

class Quantale q where
  qunit :: q
  qmult :: q -> q -> q
  qjoin :: (Finitary a) => (a -> q) -> q
qempty :: (Quantale q) => q
qempty = qjoin absurd
