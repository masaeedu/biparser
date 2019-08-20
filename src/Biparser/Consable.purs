module Biparser.Consable where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe)
import Data.String (codePointFromChar) as S
import Data.String.CodeUnits (uncons) as S
import Data.String.NonEmpty as NES

class Unconsable c g a | c -> g a
  where
  uncons :: c -> g { head :: a, tail :: c }

instance unconsableString :: Unconsable String Maybe Char
  where
  uncons = S.uncons

instance unconsableArray :: Unconsable (Array a) Maybe a
  where
  uncons = A.uncons

class Consable c a | c -> a
  where
  cons :: a -> c -> c

instance consableString :: Consable String Char
  where
  cons c s = NES.toString $ NES.cons (S.codePointFromChar c) s

instance consableArray :: Consable (Array a) a
  where
  cons = A.cons
