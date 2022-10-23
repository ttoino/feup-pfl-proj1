-- | A polynomial implementation and their common operations
module Data.Polynomial where

import ClassesAndTypes (Coefficient, Differentiable (..), Exponent, Variable)
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (sortOn)
import Data.Map (Map, empty, fromList, insertWith, toList, unionWith)
import Data.Monomial (Monomial (..))
import Data.Ord (Down (..))

-- | Represents a polynomial
newtype Polynomial
  = -- | Creates a new polynomial from its monomials
    Polynomial [Monomial]
  deriving (Eq)

-- | Implements most Num operations for monomials
--
-- signum is not implemented as it does not have a sensible meaning
-- for polynomials
instance Num Polynomial where
  x + y = normalize $ x !+ y

  x * y = normalize $ x !* y

  abs (Polynomial x) = Polynomial $ map abs x

  signum = error "Not implemented"

  fromInteger x = Polynomial [fromInteger x]

  negate (Polynomial x) = Polynomial $ map negate x

-- | Implements polynomial differentiation
instance Differentiable Polynomial where
  p // v = normalize $ p !// v

-- | Implements pretty printing of polynomials
instance Show Polynomial where
  show (Polynomial []) = ""
  show (Polynomial [m]) = show m
  show (Polynomial (m : (Monomial c e) : ms)) = show m ++ showSign c ++ show (Polynomial (abs (Monomial c e) : ms))
    where
      showSign c = if c < 0 then " - " else " + "

-- | Implements creating a polynomial from a string
--
-- Has the same limitations as read :: Monomial
instance Read Polynomial where
  readsPrec _ s = [(Polynomial $ read s, "")]

-- | Adds two polynomials without normalizing the result
(!+) :: Polynomial -> Polynomial -> Polynomial
Polynomial x !+ Polynomial y = Polynomial $ x ++ y

-- | Subtracts two polynomials without normalizing the result
(!-) :: Polynomial -> Polynomial -> Polynomial
x !- y = x !+ negate y

-- | Multiplies two polynomials without normalizing the result
(!*) :: Polynomial -> Polynomial -> Polynomial
Polynomial x !* Polynomial y = Polynomial [i * j | i <- x, j <- y]

-- | Differentiates a polynomial without normalizing the result
(!//) :: Polynomial -> Variable -> Polynomial
Polynomial p !// v = Polynomial $ map (// v) p

-- | Normalizes a polynomial
--
-- Adds alls monomials with the same degree and sorts them
normalize :: Polynomial -> Polynomial
normalize (Polynomial p) = Polynomial $ sortOn Down [Monomial c exps | (exps, c) <- toList (normalizeHelper p), c /= 0]
  where
    normalizeHelper :: [Monomial] -> Map (Map Variable Exponent) Coefficient
    normalizeHelper [] = empty
    normalizeHelper ((Monomial c exps) : xs) = unionWith (+) (fromList [(exps, c)]) (normalizeHelper xs)
