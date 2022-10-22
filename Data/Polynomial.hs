module Data.Polynomial where

import ClassesAndTypes (Differentiable (..), Exponent, Variable)
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (sortOn)
import Data.Map (Map, empty, fromList, insertWith, toList, unionWith)
import Data.Monomial (Monomial (..))
import Data.Ord (Down (..))

newtype Polynomial = Polynomial [Monomial] deriving (Eq)

instance Num Polynomial where
  Polynomial x + Polynomial y = normalize $ Polynomial $ x ++ y

  Polynomial x * Polynomial y = normalize $ Polynomial [i * j | i <- x, j <- y]

  abs (Polynomial x) = Polynomial $ map abs x

  signum = error "Not implemented"

  fromInteger x = Polynomial [fromInteger x]

  negate (Polynomial x) = Polynomial $ map negate x

instance Differentiable Polynomial where
  Polynomial p // v = normalize $ Polynomial $ map (// v) p

instance Show Polynomial where
  show (Polynomial []) = ""
  show (Polynomial [m]) = show m
  show (Polynomial (m : (Monomial c e) : ms)) = show m ++ showSign c ++ show (Polynomial (abs (Monomial c e) : ms))
    where
      showSign c = if c < 0 then " - " else " + "

instance Read Polynomial where
  readsPrec _ s = [(Polynomial $ read s, "")]

normalize :: Polynomial -> Polynomial
normalize (Polynomial p) = Polynomial $ sortOn Down [Monomial c exps | (exps, c) <- toList (normalizeHelper p), c /= 0]
  where
    normalizeHelper [] = empty
    normalizeHelper ((Monomial c exps) : xs) = unionWith (+) (fromList [(exps, c)]) (normalizeHelper xs)
