module Biparser.Optics where

import Prelude

import Biparser.Consable (class Consable, class Unconsable, cons, uncons)
import Biparser.Profunctor (class Lazy2, class Monoidal, defer2, zip)
import Data.Either (Either(..))
import Data.Lens (Prism', prism)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))

type Traversal s t a b = forall p. Monoidal p => Lazy2 p => p a b -> p s t
type Traversal' s a = Traversal s s a a

type ChoiceTraversal s t a b = forall p. Monoidal p => Lazy2 p => Choice p => p a b -> p s t
type ChoiceTraversal' s a = ChoiceTraversal s s a a

unconsed :: forall c a.
  Consable c a =>
  Unconsable c Maybe a =>
  Prism' c (a /\ c)
unconsed = prism build match
  where
  build = uncurry cons
  match xs =
    uncons xs
    # maybe (Left xs) (\{ head, tail } -> Right $ head /\ tail)

eachArray :: forall a. ChoiceTraversal' (Array a) a
eachArray p = unconsed $ zip p $ defer2 \_ -> (eachArray p)

eachTuple :: forall a. Traversal' (a /\ a) a
eachTuple p = zip p p
