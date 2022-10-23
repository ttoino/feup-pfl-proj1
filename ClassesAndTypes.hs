module ClassesAndTypes where

import Data.Natural (Natural (..))

-- | Represents a variable exponent in a monomial
type Exponent = Natural

-- | Represents a coefficient in a monomial
type Coefficient = Double

-- | Represents a variable in a monomial
type Variable = Char

-- | Represents a differentiable type
class Differentiable a where
  -- | Differentiates an expression by a variable
  (//) :: a -> Variable -> a
