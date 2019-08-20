module Biparser.Profunctor where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Apply (lift2)
import Data.Either (Either(..), either)
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

class Profunctor p <= Discerning p
  where
  choose :: forall i i' o o'. p i o -> p i' o' -> p (Either i i') (Either o o')

class Discerning p <= Picky p
  where
  default :: forall i o. p i o

instance discerningStar :: Functor f => Discerning (Star f)
  where
  choose (Star fa) (Star fb) = Star $ either (map Left <<< fa) (map Right <<< fb)

instance pickyStar :: Alternative f => Picky (Star f)
  where
  default = Star $ \_ -> empty

instance discerningJoker :: Alt f => Discerning (Joker f)
  where
  choose (Joker fa) (Joker fb) = Joker $ (Left <$> fa) <|> (Right <$> fb)

instance pickyJoker :: Alternative f => Picky (Joker f)
  where
  default = Joker $ empty

class Lazy2 p
  where
  defer2 :: forall x y. (Unit -> p x y) -> p x y
