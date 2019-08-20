module Biparser where

import Prelude

import Biparser.Profunctor (class Monoidal, constant, zip)
import Control.Monad.State (StateT(..))
import Control.Monad.Writer (WriterT(..))
import Data.Lens (left, right)
import Data.Maybe (Maybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Joker (Joker(..))
import Data.Profunctor.Star (Star(..))

jokerState = Joker <<< StateT
starWriter = Star <<< map WriterT

newtype Biparser i o =
  Biparser
  { parse :: Joker (StateT String Maybe) i o
  , print :: Star (WriterT String Maybe) i o
  }

type Biparser' v = Biparser v v

instance functorBiparser :: Functor (Biparser i)
  where
  map f (Biparser b) = Biparser { parse, print }
    where
    parse = map f b.parse
    print = map f b.print

instance profunctorBiparser :: Profunctor Biparser
  where
  dimap f g (Biparser b) = Biparser { parse, print }
    where
    parse = dimap f g b.parse
    print = dimap f g b.print

instance choiceBiparser :: Choice Biparser
  where
  left (Biparser b) = Biparser { parse, print }
    where
    parse = left b.parse
    print = left b.print

  right (Biparser b) = Biparser { parse, print }
    where
    parse = right b.parse
    print = right b.print

instance monoidalBiparser :: Monoidal Biparser
  where
  constant = Biparser { parse, print }
    where
    parse = constant
    print = constant
  zip (Biparser b1) (Biparser b2) = Biparser { parse, print }
    where
    parse = zip b1.parse b2.parse
    print = zip b1.print b2.print
