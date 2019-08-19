module Main where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice)
import Data.String.CodeUnits (singleton, uncons)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)

swapEither :: forall a b. Either a b -> Either b a
swapEither = case _ of
  Left a -> Right a
  Right a -> Left a

map' :: forall a b x. (a -> b) -> Maybe (a /\ x) -> Maybe (b /\ x)
map' = map <<< lmap

newtype Biparser i o =
  Biparser
  { parse :: String -> Maybe (o /\ String) -- \_ -> StateT String Maybe o
  , print :: i -> Maybe (o /\ String)      -- \i -> WriterT String Maybe o
  }

type Biparser' v = Biparser v v

instance functorBiparser :: Functor (Biparser i)
  where
  map f (Biparser b) = Biparser { parse, print }
    where
    parse = map' f <<< b.parse
    print = map' f <<< b.print

-- instance applyBiparser :: Apply (Biparser i)
--   where
--   apply (Biparser {  }) (Biparser b) = Biparser { parse, print }
--     where
--     parse = map' f <<< b.parse
--     print = map' f <<< b.print

instance profunctorBiparser :: Profunctor Biparser
  where
  dimap f g (Biparser b) = Biparser { parse, print }
    where
    parse = map' g <<< b.parse
    print = map' g <<< b.print <<< f

biparserLeft :: forall a b c. Biparser a b -> Biparser (Either a c) (Either b c)
biparserLeft (Biparser b) = Biparser { parse, print }
  where
  parse = b.parse >>> map' Left
  print = case _ of
    Left  a -> map' Left $ b.print a
    Right c -> Just $ (_ /\ mempty) $ Right $ c

biparserRight :: forall a b c. Biparser a b -> Biparser (Either c a) (Either c b)
biparserRight = dimap swapEither swapEither <<< biparserLeft

instance choiceBiparser :: Choice Biparser
  where
  left = biparserLeft
  right = biparserRight

char :: Biparser' Char
char = Biparser { parse, print }
  where
  parse s = do
    { head, tail } <- uncons s
    pure $ Tuple head tail
  print c = Just $ Tuple c (singleton c)

-- string :: String -> Biparser' String
-- string s =

main :: Effect Unit
main = do
  log "🍝"
