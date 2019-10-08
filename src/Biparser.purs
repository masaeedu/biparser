module Biparser where

import Prelude

import Biparser.Profunctor (class Monoidal, constant, zip)
import Control.Monad.State (StateT(..))
import Control.Monad.Writer (WriterT(..))
import Data.Lens (left, right)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
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

derive instance newtypeBiparser :: Newtype (Biparser i o) _

type Biparser' v = Biparser v v

instance functorBiparser :: Functor (Biparser i)
  where
  map f (Biparser b) = Biparser { parse, print }
    where
    parse = map f b.parse
    print = map f b.print

instance applyBiparser :: Apply (Biparser i)
  where
  apply (Biparser f) (Biparser b) = Biparser { parse, print }
    where
    parse = apply f.parse b.parse
    print = apply f.print b.print

instance applicativeBiparser :: Applicative (Biparser i)
  where
  pure a = Biparser { parse: pure a, print: pure a }

instance bindBiparser :: Bind (Biparser i)
  where
  bind (Biparser b) f = Biparser { parse, print }
    where
    parse = b.parse >>= (_.parse <<< un Biparser <<< f)
    print = b.print >>= (_.print <<< un Biparser <<< f)

instance monadBiparser :: Monad (Biparser i)

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
