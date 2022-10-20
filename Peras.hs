{-# LANGUAGE InstanceSigs #-}

module Peras where

import Data.Char (chr, ord)
import Data.Map
  ( Map,
    empty,
    findWithDefault,
    fromList,
    toList,
    unionWith,
    update,
  )

data PositiveOrZero = Zero | Suc PositiveOrZero deriving (Eq, Ord)

instance Enum PositiveOrZero where
  toEnum :: Int -> PositiveOrZero
  toEnum n
    | n == 0 = Zero
    | n > 0 = Suc $ toEnum (n - 1)
    | otherwise = error "Cannot be negative"
  fromEnum :: PositiveOrZero -> Int
  fromEnum Zero = 0
  fromEnum (Suc n) = 1 + fromEnum n

instance Num PositiveOrZero where
  (+) :: PositiveOrZero -> PositiveOrZero -> PositiveOrZero
  a + b = toEnum $ fromEnum a + fromEnum b
  (*) :: PositiveOrZero -> PositiveOrZero -> PositiveOrZero
  a * b = toEnum $ fromEnum a * fromEnum b
  (-) :: PositiveOrZero -> PositiveOrZero -> PositiveOrZero
  a - b
    | number < 0 = error "Negative difference"
    | otherwise = toEnum number
    where
      number = fromEnum a - fromEnum b
  abs :: PositiveOrZero -> PositiveOrZero
  abs n = n
  signum :: PositiveOrZero -> PositiveOrZero
  signum p = 1
  fromInteger :: Integer -> PositiveOrZero
  fromInteger n
    | n < 0 = error "Cannot be negative"
    | n == 0 = Zero
    | otherwise = Suc (fromInteger (n - 1))
  negate :: PositiveOrZero -> PositiveOrZero
  negate = error "Cannot negate value"

instance Show PositiveOrZero where
  show :: PositiveOrZero -> String
  show p = "Suc(" ++ [chr $ ord '0' + fromEnum p] ++ ")"

---------------------END PositiveOrZero DATA TYPE DEFINITION------------------------------

type Exponent = PositiveOrZero

type Coefficient = Double

type Monomial = (Coefficient, Map Char Exponent)

type Polynomial = [Monomial]

----------------------END TYPE DECLARATIONS-----------------------------------------

(*=) :: Coefficient -> Exponent -> Coefficient
c *= e = c * fromIntegral (fromEnum e)

------------------------------------------------------------------------------------

normalize :: Polynomial -> Polynomial
normalize p = [(c, exps) | (exps, c) <- toList (normalizeHelper p), c /= 0]
  where
    normalizeHelper [] = empty
    normalizeHelper ((c, exps) : xs) = unionWith (+) (fromList [(exps, c)]) (normalizeHelper xs)

derivate :: Polynomial -> Char -> Polynomial
derivate p c = [derivateMonomial m c | m <- p]

derivateMonomial :: Monomial -> Char -> Monomial
derivateMonomial (coef, exps) c =
  ( newCoef,
    if newCoef == 0
      then empty
      else update (\a -> if a == Suc Zero then Nothing else Just (a - 1)) c exps
  )
  where
    newCoef = coef *= findWithDefault 0 c exps