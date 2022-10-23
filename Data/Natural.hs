-- | A natural number implementation and their common operations
module Data.Natural where

import GHC.Real ((%))

-- | Represents a natural number, i.e. an integer greater than zero
data Natural
  = -- | Represents the number one
    One
  | -- | Represents the number that comes after another
    Suc Natural
  deriving (Eq, Ord)

-- | Implements all enum operations on naturals
instance Enum Natural where
  toEnum n
    | n == 1 = One
    | n > 1 = Suc $ toEnum (n - 1)
    | otherwise = error "Cannot be negative"

  fromEnum One = 1
  fromEnum (Suc n) = 1 + fromEnum n

-- | Implements most num operations on naturals
instance Num Natural where
  a + b = toEnum $ fromEnum a + fromEnum b

  a * b = toEnum $ fromEnum a * fromEnum b

  a - b
    | number <= 0 = error "Negative difference"
    | otherwise = toEnum number
    where
      number = fromEnum a - fromEnum b

  abs n = n

  signum p = 1

  fromInteger n
    | n < 1 = error "Must be positive"
    | n == 1 = One
    | otherwise = Suc (fromInteger (n Prelude.- 1))

  negate = error "Cannot negate value"

-- | Adds a way to show naturals
instance Show Natural where
  show = show . fromEnum

-- | Implements all real operations on naturals
instance Real Natural where
  toRational x = toInteger x % 1

-- | Implements all integral operations on naturals
instance Integral Natural where
  quotRem x y = (toEnum q, toEnum r)
    where
      (q, r) = quotRem (fromEnum x) (fromEnum y)

  toInteger = toInteger . fromEnum
