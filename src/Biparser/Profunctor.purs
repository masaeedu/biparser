module Biparser.Profunctor where

import Prelude

import Control.Apply (lift2)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Joker (Joker(..))
import Data.Profunctor.Star (Star(..))
import Data.Tuple.Nested (type (/\), (/\))

class Profunctor p <= Monoidal p
  where
  constant :: p Unit Unit
  zip :: forall a b a' b'. p a b -> p a' b' -> p (a /\ a') (b /\ b')

instance monoidalJoker :: Applicative f => Monoidal (Joker f)
  where
  constant = Joker $ pure unit
  zip (Joker f) (Joker g) = Joker $ lift2 (/\) f g

instance monoidalStar :: Applicative f => Monoidal (Star f)
  where
  constant = Star pure
  zip (Star f) (Star g) = Star \(a /\ a') -> lift2 (/\) (f a) (g a')
