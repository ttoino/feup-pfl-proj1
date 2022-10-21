module Data.Natural where

import GHC.Real ((%))

data Natural = One | Suc Natural deriving (Eq, Ord)

instance Enum Natural where
  toEnum n
    | n == 1 = One
    | n > 1 = Suc $ toEnum (n - 1)
    | otherwise = error "Cannot be negative"

  fromEnum One = 1
  fromEnum (Suc n) = 1 + fromEnum n

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

instance Show Natural where
  show = show . fromEnum

instance Real Natural where
  toRational x = toInteger x % 1

instance Integral Natural where
  quotRem x y = (toEnum q, toEnum r)
    where
      (q, r) = quotRem (fromEnum x) (fromEnum y)

  toInteger = toInteger . fromEnum
