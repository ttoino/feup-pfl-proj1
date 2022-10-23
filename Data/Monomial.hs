-- | A monomial implementation and their common operations
module Data.Monomial where

import ClassesAndTypes (Coefficient, Differentiable (..), Exponent, Variable)
import Data.Char (isAsciiLower, isDigit, isSpace)
import Data.Map (Map, adjust, delete, empty, fromList, lookup, mapWithKey, member, null, toList, unionWith)
import Data.Maybe (fromJust, isNothing)
import Data.Natural (Natural (One))
import Util (showReal, showSuperscript)

-- | Represents a monomial
data Monomial
  = -- | Creates a new monomial from its coefficient and variables
    Monomial
      Coefficient
      -- ^ Represents the monomial's coefficient
      (Map Variable Exponent)
      -- ^ Represents the monomial's variables and their exponents
  deriving (Eq)

-- | Implements all Num operations for monomials
--
-- Adding/subtracting two monomials with different degrees will fail
instance Num Monomial where
  Monomial c1 m1 + Monomial c2 m2
    | m1 == m2 = Monomial (c1 + c2) m1
    | otherwise = error "Can't add monomials with different degrees"

  Monomial c1 m1 * Monomial c2 m2 = Monomial (c1 * c2) (unionWith (+) m1 m2)

  abs (Monomial n m) = Monomial (abs n) m

  signum (Monomial n m) = Monomial (signum n) empty

  fromInteger n = Monomial (fromInteger n) empty

  negate (Monomial n m) = Monomial (-n) m

-- | Implements monomial differentiation
instance Differentiable Monomial where
  Monomial n m // v = case Data.Map.lookup v m of
    Nothing -> 0
    Just One -> Monomial n (delete v m)
    Just exp -> Monomial (n * fromInteger (toInteger exp)) (adjust (\x -> x - One) v m)

-- | Implements pretty printing of monomials
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

-- | Implements monomial ordering
--
-- Monomials will be ordered by degree first, then by coefficient
--
-- Degree ordering is lexicographic, meaning x*y is greater than y,
-- and x^3*y is greater than x^2*y^4
instance Ord Monomial where
  Monomial ca ea <= Monomial cb eb
    | ea == eb = ca <= cb
    | otherwise = or $ zipWith compare (toList ea) (toList eb)
    where
      compare (v1, e1) (v2, e2)
        | v1 == v2 = e1 <= e2
        | otherwise = v1 >= v2

-- | Implements creating a monomial (or a monomial list) from a string
--
-- Will fail if the string does not match an expected format roughly
-- equivalent to the regular expression `[+-]?\d*\.?\d*(\*?[a-z]^?\d*)*`
-- with some whitespace also ignored
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
      readCoefficient cs = case f of
        "" -> (1, s)
        f -> (read f, s)
        where
          (f, s) = span (\c -> isDigit c || c == '.') cs

      readVars :: String -> (Map Variable Exponent, String)
      readVars vs = (fromList $ readVarsHelper f, s)
        where
          (f, s) = span (\c -> isSpace c || isDigit c || isAsciiLower c || c == '*' || c == '^') vs

          readVarsHelper :: String -> [(Variable, Exponent)]
          readVarsHelper "" = []
          readVarsHelper (' ' : s) = readVarsHelper s
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

-- | Checks if two monomials have the same degree
(~=) :: Monomial -> Monomial -> Bool
Monomial _ m1 ~= Monomial _ m2 = m1 == m2

-- | Checks if two monomials have different degrees
(~/=) :: Monomial -> Monomial -> Bool
m1 ~/= m2 = not $ m1 ~= m2
