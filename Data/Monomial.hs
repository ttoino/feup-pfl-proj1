module Data.Monomial where

import Data.Char (isAsciiLower, isDigit, isSpace)
import Data.Map (Map, adjust, delete, empty, fromList, lookup, mapWithKey, member, null, unionWith)
import Data.Maybe (fromJust, isNothing)
import Data.Natural (Natural (One))
import Util (showReal, showSuperscript)

type Exponent = Natural

type Coefficient = Double

type Variable = Char

class Differentiable a where
  (//) :: a -> Variable -> a

data Monomial = Monomial Coefficient (Map Variable Exponent) deriving (Eq)

instance Num Monomial where
  Monomial c1 m1 + Monomial c2 m2
    | m1 == m2 = Monomial (c1 + c2) m1
    | otherwise = error "Can't add monomials with different degrees"

  Monomial c1 m1 * Monomial c2 m2 = Monomial (c1 * c2) (unionWith (+) m1 m2)

  abs (Monomial n m) = Monomial (abs n) m

  signum (Monomial n m) = Monomial (signum n) empty

  fromInteger n = Monomial (fromInteger n) empty

  negate (Monomial n m) = Monomial (-n) m

instance Differentiable Monomial where
  Monomial n m // v = case Data.Map.lookup v m of
    Nothing -> 0
    Just One -> Monomial n (delete v m)
    Just exp -> Monomial (n * fromInteger (toInteger exp)) (adjust (\x -> x - One) v m)

instance Show Monomial where
  show (Monomial c m)
    | Data.Map.null m = showReal c
    | c == 1 = showVars m
    | c == (-1) = '-' : showVars m
    | otherwise = showReal c ++ showVars m
    where
      showVars m = concat $ mapWithKey showVar m
      showVar v 1 = [v]
      showVar v e = v : showSuperscript (toInteger e)

instance Ord Monomial where
  Monomial ca ea <= Monomial cb eb
    | ea == eb = ca <= cb
    | otherwise = ea <= eb

instance Read Monomial where
  readsPrec _ s = [readMonomial s]
    where
      readMonomial :: String -> (Monomial, String)
      readMonomial s' = (Monomial coeff vars, s''')
        where
          (coeff, s'') = readCoefficient s'
          (vars, s''') = readVars s''

      readCoefficient :: String -> (Coefficient, String)
      readCoefficient ('-' : cs) = (-f, s)
        where
          (f, s) = readCoefficient cs
      readCoefficient ('+' : cs) = readCoefficient cs
      readCoefficient (' ' : cs) = readCoefficient cs
      readCoefficient cs = (read f, s)
        where
          (f, s) = span (\c -> isDigit c || c == '.') cs

      readVars :: String -> (Map Variable Exponent, String)
      readVars vs = (fromList $ readVarsHelper f, s)
        where
          (f, s) = span (\c -> isSpace c || isDigit c || isAsciiLower c || c == '*' || c == '^') vs

          readVarsHelper :: String -> [(Variable, Exponent)]
          readVarsHelper "" = []
          readVarsHelper s = (v, e) : readVarsHelper s''
            where
              readVar :: String -> (Variable, String)
              readVar "" = error "No read"
              readVar (v : vs)
                | v == '*' || v == ' ' = readVar vs
                | isAsciiLower v = (v, vs)
                | otherwise = error "No read"

              (v, s') = readVar s

              readExponent :: String -> Exponent
              readExponent "" = One
              readExponent ('^' : es) = readExponent es
              readExponent (' ' : es) = readExponent es
              readExponent e = fromInteger $ read e

              (e, s'') = (readExponent es, s'')
                where
                  (es, s'') = span (\c -> isSpace c || isDigit c || c == '^') s'

  readList s = [(helper s, "")]
    where
      helper "" = []
      helper s = m : helper s'
        where
          (m, s') = head (reads s :: [(Monomial, String)])
