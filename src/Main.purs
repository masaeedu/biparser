module Main where

import Prelude

import Biparser (Biparser(..), Biparser', jokerState, starWriter)
import Biparser.Consable (cons)
import Biparser.Optics (unconsed)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

next :: Biparser' Char
next = Biparser { parse, print }
  where
  parse = jokerState $ \s -> preview unconsed s
  print = starWriter $ \c -> Just $ (c /\ (cons c ""))

main :: Effect Unit
main = do
  log "🍝"
