{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LabelledPolynomial where

import Polynomial (Polynomial)
import Util (showSubscript)

type Variable = Char

data LabelledPolynomial = Polynomial :@: [Variable]
  deriving (Eq)

class Derivable a b where
  (//) :: a -> b -> a

instance Show LabelledPolynomial where
  show (ms :@: v) = foldr ($) (show ms) [replaceS ('x' ++ showSubscript x) var | (x, var) <- zip [1 ..] v]

instance Read LabelledPolynomial where
  readsPrec _ s = [(readLabelledPolynomial s, "")]
    where
      readLabelledPolynomial s = vars :@: readMonomials s'
        where
          readMonomials :: String -> [Monomial]
          readMonomials [] = []
          readMonomials m = readCoefficient c :^: foldr (zipWith (+)) [0 | x <- vars] v : readMonomials t
            where
              (c, r)
                | head m == '-' || head m == '+' = (head m : takeWhile isDigit (tail m), dropWhile isDigit (tail m))
                | otherwise = span isDigit m
              (v, t) = readVars r
              readCoefficient [] = 1
              readCoefficient "-" = -1
              readCoefficient ('+' : o) = readCoefficient o
              readCoefficient x = read x

          readVars :: String -> ([[Exponent]], String)
          readVars ('*' : r) = readVars r
          readVars (v : r) | isLetter v = ([if v == var then readExponent e else 0 | var <- vars] : vs, tt)
            where
              (vs, tt) = readVars t
              (e, t) = span (\x -> isDigit x || x == '^') r
              readExponent [] = 1
              readExponent ('^' : xs) = readExponent xs
              readExponent e = read e
          readVars r = ([[0 | x <- vars]], r)

          vars = nub $ sort $ filter isLetter s'

          s' = filter (not . isSpace) s

instance Num LabelledPolynomial where
  (va :@: ma) + (vb :@: mb)
    | va /= vb = convertBasis (va :@: ma) b + convertBasis (vb :@: mb) b
    | otherwise = b :@: (ma `addMonomials` mb)
    where
      b = va `union` vb
      [] `addMonomials` m = m
      m `addMonomials` [] = m
      (ma : mas) `addMonomials` mb = (ma `addMatching` mb) : mas `addMonomials` filter (not . sameDegree ma) mb

  (va :@: ma) * (vb :@: mb)
    | va /= vb = convertBasis (va :@: ma) b * convertBasis (vb :@: mb) b
    | otherwise = b :@: [a * b | a <- ma, b <- mb]
    where
      b = va `union` vb

  abs (v :@: m) = v :@: map abs m

  signum (v :@: m) = v :@: map signum m

  fromInteger i = "" :@: [fromInteger i]

  negate (v :@: m) = v :@: map negate m

instance Derivable Monomial Int where
  (c :^: e) // d
    | d == -1 || ex == 0 = 0 :^: [0 | _ <- e]
    | otherwise = (c * fromIntegral ex) :^: [if i == d then x - 1 else x | (x, i) <- zip e [0 ..]]
    where
      ex = e !! d

instance Derivable LabelledPolynomial Variable where
  (v :@: m) // d = v :@: map (// i) m
    where
      i = case elemIndex d v of
        Just i -> i
        Nothing -> -1

(:@@:) :: LabelledPolynomial -> [Variable] -> LabelledPolynomial
(ms :@: v) :@@: b
  | v == b = ms :@: v
  | intersect v b /= v = error "Invalid basis"
  | otherwise = map convertMonomial ms :@: b
  where
    convertMonomial (c :^: es) = c :^: [head $ [e | (ov, e) <- zip v es, ov == nv] ++ [0] | nv <- b]

addMatching :: Monomial -> [Monomial] -> Monomial
m `addMatching` [] = m
ma `addMatching` (mb : ms)
  | sameDegree ma mb = (ma + mb) `addMatching` ms
  | otherwise = ma `addMatching` ms

normalize :: LabelledPolynomial -> LabelledPolynomial
normalize (v :@: m) = nv :@: reverse (sort nm)
  where
    (nv :@: nm) = convertBasis (v :@: normalizeMonomials m) $ sort v
    normalizeMonomials [] = []
    normalizeMonomials (0 :^: _ : ms) = normalizeMonomials ms
    normalizeMonomials (m : ms) = addMatching m ms : normalizeMonomials (filter (not . sameDegree m) ms)
