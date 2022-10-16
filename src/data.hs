{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Char (isDigit, isLetter, isSpace)
import Data.List (elemIndex, intercalate, intersect, intersperse, nub, sort, sortBy, union)
import Data.Maybe (fromJust)

type Variable = Char

type Exponent = Integer

type Coefficient = Double

data Monomial = Coefficient :^: [Exponent] deriving (Eq)

data Polynomial = [Variable] :@: [Monomial] deriving (Eq)

class Derivable a b where
  (//) :: a -> b -> a

showExponent :: Exponent -> String
showExponent e = map ((exponents !!) . fromInteger) (reverse (digits e))
  where
    exponents = "⁰¹²³⁴⁵⁶⁷⁸⁹"
    digits 0 = []
    digits n = n `mod` 10 : digits (n `div` 10)

showCoefficient :: Coefficient -> String
showCoefficient c
  | dec == ".0" = int
  | otherwise = s
  where
    s = show c
    (int, dec) = span (/= '.') s

instance Show Polynomial where
  show (v :@: ms) = showMonomials ms
    where
      showMonomials [] = ""
      showMonomials [m] = showMonomial m
      showMonomials (m : (c :^: e) : ms) = showMonomial m ++ showSign c ++ showMonomials ((abs c :^: e) : ms)
      showSign c = if c < 0 then " - " else " + "
      showMonomial (1 :^: e) | any (> 0) e = showVars e
      showMonomial ((-1) :^: e) | any (> 0) e = '-' : showVars e
      showMonomial (c :^: e) = showCoefficient c ++ showVars e
      showVars e = concat (zipWith showVar v e)
      showVar v 0 = []
      showVar v 1 = [v]
      showVar v e = v : showExponent e

instance Read Polynomial where
  readsPrec _ s = [(readPolynomial s, "")]
    where
      readPolynomial s = vars :@: readMonomials s'
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

instance Num Polynomial where
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

instance Num Monomial where
  (ca :^: ea) + (cb :^: eb)
    | ea /= eb = error "Can't add monomials with different exponents"
    | otherwise = (ca + cb) :^: ea

  (ca :^: ea) * (cb :^: eb) = (ca * cb) :^: zipWith (+) ea eb

  abs (c :^: e) = abs c :^: e

  signum (c :^: e) = signum c :^: []

  fromInteger i = fromInteger i :^: []

  negate (c :^: e) = (-c) :^: e

instance Ord Monomial where
  (ca :^: ea) <= (cb :^: eb)
    | ea == eb = ca <= cb
    | otherwise = ea <= eb

instance Derivable Monomial Int where
  (c :^: e) // d
    | d == -1 || ex == 0 = 0 :^: [0 | _ <- e]
    | otherwise = (c * fromIntegral ex) :^: [if i == d then x - 1 else x | (x, i) <- zip e [0 ..]]
    where
      ex = e !! d

instance Derivable Polynomial Variable where
  (v :@: m) // d = v :@: map (// i) m
    where
      i = case elemIndex d v of
        Just i -> i
        Nothing -> -1

convertBasis :: Polynomial -> [Variable] -> Polynomial
convertBasis (v :@: ms) b
  | v == b = v :@: ms
  | intersect v b /= v = error "Invalid basis"
  | otherwise = b :@: map convertMonomial ms
  where
    convertMonomial (c :^: es) = c :^: [head $ [e | (ov, e) <- zip v es, ov == nv] ++ [0] | nv <- b]

sameDegree :: Monomial -> Monomial -> Bool
sameDegree (ca :^: ea) (cb :^: eb) = ea == eb

addMatching :: Monomial -> [Monomial] -> Monomial
m `addMatching` [] = m
ma `addMatching` (mb : ms)
  | sameDegree ma mb = (ma + mb) `addMatching` ms
  | otherwise = ma `addMatching` ms

normalize :: Polynomial -> Polynomial
normalize (v :@: m) = nv :@: reverse (sort nm)
  where
    (nv :@: nm) = convertBasis (v :@: normalizeMonomials m) $ sort v
    normalizeMonomials [] = []
    normalizeMonomials (0 :^: _ : ms) = normalizeMonomials ms
    normalizeMonomials (m : ms) = addMatching m ms : normalizeMonomials (filter (not . sameDegree m) ms)

splitOnAny :: Eq a => [a] -> [a] -> [[a]]
splitOnAny x = splitBy (`elem` x)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = splitBy (== x)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f [] = []
splitBy f (x : xs) | f x = splitBy f xs
splitBy f a = p : splitBy f a'
  where
    (p, a') = break f a

main =
  do
    putStr "Input a polynomial: "
    input <- getLine
    let a = read input :: Polynomial
    putStrLn "You inputted the polynomial:"
    putStr $ show a
    putStr " = "
    let norm_a = normalize a
    print norm_a

    putStr "Input another polynomial: "
    input <- getLine
    let b = read input :: Polynomial
    putStrLn "You inputted the polynomial:"
    putStr $ show b
    putStr " = "
    let norm_b = normalize b
    print norm_b

    let sum = a + b
    putStrLn "Their sum:"
    putStr $ show sum
    putStr " = "
    let norm_sum = normalize sum
    print norm_sum

    let product = a * b
    putStrLn "Their product:"
    putStr $ show product
    putStr " = "
    let norm_product = normalize product
    print norm_product
