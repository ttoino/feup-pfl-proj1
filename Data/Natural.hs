{-# LANGUAGE InstanceSigs #-}
module Data.Natural where

import GHC.Real ((%))

data Natural = One | Suc Natural deriving (Eq, Ord)

instance Enum Natural where
  toEnum :: Int -> Natural
  toEnum n
    | n == 1 = One
    | n > 1 = Suc $ toEnum (n - 1)
    | otherwise = error "Cannot be negative"

  fromEnum :: Natural -> Int
  fromEnum One = 1
  fromEnum (Suc n) = 1 + fromEnum n

instance Num Natural where
  (+) :: Natural -> Natural -> Natural
  a + b = toEnum $ fromEnum a + fromEnum b

  (*) :: Natural -> Natural -> Natural
  a * b = toEnum $ fromEnum a * fromEnum b

  (-) :: Natural -> Natural -> Natural
  a - b
    | number <= 0 = error "Negative difference"
    | otherwise = toEnum number
    where
      number = fromEnum a - fromEnum b

  abs :: Natural -> Natural
  abs n = n

  signum :: Natural -> Natural
  signum p = 1

  fromInteger :: Integer -> Natural
  fromInteger n
    | n < 1 = error "Must be positive"
    | n == 1 = One
    | otherwise = Suc (fromInteger (n Prelude.- 1))

  negate :: Natural -> Natural
  negate = error "Cannot negate value"

instance Show Natural where
  show :: Natural -> String
  show = show . fromEnum

instance Real Natural where
  toRational :: Natural -> Rational
  toRational x = toInteger x % 1

instance Integral Natural where
  quotRem :: Natural -> Natural -> (Natural, Natural)
  quotRem x y = (toEnum q, toEnum r)
    where
      (q, r) = quotRem (fromEnum x) (fromEnum y)

  toInteger :: Natural -> Integer
  toInteger = toInteger . fromEnum
