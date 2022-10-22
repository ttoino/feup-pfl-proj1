module ClassesAndTypes where

import Data.Natural (Natural (..))

type Exponent = Natural

type Coefficient = Double

type Variable = Char

class Differentiable a where
  (//) :: a -> Variable -> a
