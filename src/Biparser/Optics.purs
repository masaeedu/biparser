module Biparser.Optics where

import Prelude

import Biparser.Consable (class Consable, class Unconsable, cons, uncons)
import Biparser.Profunctor (class Monoidal, zip)
import Data.Either (Either(..))
import Data.Lens (Prism', prism)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))

type Traversal s t a b = forall p. Monoidal p => p a b -> p s t
type Traversal' s a = Traversal s s a a

type ChoiceTraversal s t a b = forall p. Monoidal p => Choice p => p a b -> p s t
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

traversed :: forall a. ChoiceTraversal' (Array a) a
traversed p = unconsed $ zip p (traversed p)
