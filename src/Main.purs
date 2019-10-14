module Main where

import Prelude

import Biparser (Biparser(..), Biparser', biparse, biprint, jokerState, starWriter)
import Biparser.Consable (cons)
import Biparser.Optics (both, unconsed)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso, Iso', Optic', _Just, iso, preview, re)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class.Console (logShow)

comap :: forall p a a' b. Choice p => Cochoice p => (a -> Maybe a') -> p a' b -> p a b
comap f = re _Just <<< lcmap ((=<<) f) <<< _Just

data Digit
  = D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9

derive instance genericDigit :: Generic Digit _

instance showDigit :: Show Digit
  where
  show = genericShow

d2c :: Digit -> Char
d2c D0 = '0'
d2c D1 = '1'
d2c D2 = '2'
d2c D3 = '3'
d2c D4 = '4'
d2c D5 = '5'
d2c D6 = '6'
d2c D7 = '7'
d2c D8 = '8'
d2c D9 = '9'

c2d :: Char -> Maybe Digit
c2d '0' = Just D0
c2d '1' = Just D1
c2d '2' = Just D2
c2d '3' = Just D3
c2d '4' = Just D4
c2d '5' = Just D5
c2d '6' = Just D6
c2d '7' = Just D7
c2d '8' = Just D8
c2d '9' = Just D9
c2d _ = Nothing

data TwoChars = TwoChars Char Char

derive instance genericTwoChars :: Generic TwoChars _
instance showTwoChars :: Show TwoChars
  where
  show = genericShow

twoCharsAsTuple :: Iso' (Char /\ Char) TwoChars
twoCharsAsTuple = iso (\(c1 /\ c2) -> TwoChars c1 c2) (\(TwoChars c1 c2) -> c1 /\ c2)

m2e :: forall a b. Iso (Maybe a) (Maybe b) (Either Unit a) (Either Unit b)
m2e = iso bwd fwd
  where
  fwd = case _ of
    Left _ -> Nothing
    Right x -> Just x
  bwd = case _ of
    Nothing -> Left unit
    Just x -> Right x

next :: Biparser' Char
next = Biparser { parse, print }
  where
  parse = jokerState $ \s -> preview unconsed s
  print = starWriter $ \c -> Just $ c /\ cons c ""

digitAsChar :: forall p. Choice p => Cochoice p => Optic' p Char Digit
digitAsChar = comap c2d <<< rmap d2c

digit :: Biparser' Digit
digit = (re digitAsChar) next

test :: Biparser Char TwoChars
test = do
  c1 <- next
  c2 <- next
  pure $ TwoChars c1 c2

main :: Effect Unit
main = do
  logShow $ biparse test "foo"                                                        -- (Just (Tuple (TwoChars 'f' 'o') "o"))
  logShow $ biprint test 'x'                                                          -- (Just (Tuple (TwoChars 'x' 'x') "xx"))
  logShow $ biparse (twoCharsAsTuple <<< lcmap (\(TwoChars c1 _) -> c1) $ test) "foo" -- (Just (Tuple (Tuple 'f' 'o') "o"))

  let dc = digitAsChar digit
  let cons2 c1 c2 = c1 `cons` (c2 `cons` "")
  let twochars = cons2 <$> dc <*> dc

  logShow $ biparse twochars "1234" -- (Just (Tuple "12" "34"))
  logShow $ biparse twochars "1f34" -- Nothing

  let tupleAsTwoChars = re twoCharsAsTuple
  let fourchars = both <<< tupleAsTwoChars <<< both

  logShow $ biparse (fourchars next) "54321" -- (Just (Tuple (Tuple (TwoChars '5' '4') (TwoChars '3' '2')) "1"))
  logShow $ biprint (both digit) (D5 /\ D4)  -- (Just (Tuple (Tuple D5 D4) "54"))
