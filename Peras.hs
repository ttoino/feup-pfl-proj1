module Peras where
import Data.Map

data Positive = Zero | Suc Positive deriving (Eq, Show, Ord)

instance Enum Positive where
  toEnum :: Int -> Positive
  toEnum 0 = Zero
  toEnum n | n > 0 = Suc $ toEnum (n - 1)
  fromEnum :: Positive -> Int
  fromEnum Zero = 0
  fromEnum (Suc n) = 1 + fromEnum (n - 1)

instance Num Positive where
  (+) :: Positive -> Positive -> Positive
  a + b = a + b
  (*) :: Positive -> Positive -> Positive
  a * b = a * b
  abs :: Positive -> Positive
  abs n = n
  signum :: Positive -> Positive
  signum p = 1
  fromInteger :: Integer -> Positive
  fromInteger n
    | n < 0 = error "Cannot be negative"
    | n == 0 = Zero
    | otherwise = Suc (fromInteger n - 1)
  negate :: Positive -> Positive
  negate = error "Cannot negate value"

-----------------------------------------------------------------

type Exponent = Positive

type Coefficient = Double

type Monomial = (Coefficient, Map Char Exponent)

type Polynomial = [Monomial]