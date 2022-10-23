module Data.Polynomial where

import ClassesAndTypes (Differentiable (..), Exponent, Variable)
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (sortOn)
import Data.Map (Map, empty, fromList, insertWith, toList, unionWith)
import Data.Monomial (Monomial (..))
import Data.Ord (Down (..))

newtype Polynomial = Polynomial [Monomial] deriving (Eq)

instance Num Polynomial where
  x + y = normalize $ x !+ y

  x * y = normalize $ x !* y

  abs (Polynomial x) = Polynomial $ map abs x

  signum = error "Not implemented"

  fromInteger x = Polynomial [fromInteger x]

  negate (Polynomial x) = Polynomial $ map negate x

instance Differentiable Polynomial where
  p // v = normalize $ p !// v

instance Show Polynomial where
  show (Polynomial []) = ""
  show (Polynomial [m]) = show m
  show (Polynomial (m : (Monomial c e) : ms)) = show m ++ showSign c ++ show (Polynomial (abs (Monomial c e) : ms))
    where
      showSign c = if c < 0 then " - " else " + "

instance Read Polynomial where
  readsPrec _ s = [(Polynomial $ read s, "")]

(!+) :: Polynomial -> Polynomial -> Polynomial
Polynomial x !+ Polynomial y = Polynomial $ x ++ y

(!-) :: Polynomial -> Polynomial -> Polynomial
x !- y = x !+ negate y

(!*) :: Polynomial -> Polynomial -> Polynomial
Polynomial x !* Polynomial y = Polynomial [i * j | i <- x, j <- y]

(!//) :: Polynomial -> Variable -> Polynomial
Polynomial p !// v = Polynomial $ map (// v) p

normalize :: Polynomial -> Polynomial
normalize (Polynomial p) = Polynomial $ sortOn Down [Monomial c exps | (exps, c) <- toList (normalizeHelper p), c /= 0]
  where
    normalizeHelper [] = empty
    normalizeHelper ((Monomial c exps) : xs) = unionWith (+) (fromList [(exps, c)]) (normalizeHelper xs)
