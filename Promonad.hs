-- |

module Promonad where

class (Category hom obj) => Promonad hom obj where
  lift :: (obj a, obj b) => (a -> b) -> hom a b
