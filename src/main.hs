import Data.Char (isDigit, isLetter, isSpace)
import Data.List (elemIndex, intercalate, intersect, intersperse, nub, sort, sortBy, union)
import Data.Maybe (fromJust)

type Variable = Char

type Exponent = Integer

type Coefficient = Double

type Monomial = (Coefficient, [Exponent])

type Polynomial = ([Variable], [Monomial])

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

showPolynomial :: Polynomial -> String
showPolynomial (v, ms) = intercalate " + " (map showMonomial ms)
  where
    showMonomial (1, e) | any (> 0) e = showVars e
    showMonomial (c, e) = showCoefficient c ++ showVars e
    showVars e = concat (zipWith showVar v e)
    showVar v 0 = []
    showVar v 1 = [v]
    showVar v e = v : showExponent e

convertBasis :: Polynomial -> [Variable] -> Polynomial
convertBasis (v, ms) b
  | v == b = (v, ms)
  | intersect v b /= v = error "Invalid basis"
  | otherwise = (b, map convertMonomial ms)
  where
    convertMonomial (c, es) = (c, [head $ [e | (ov, e) <- zip v es, ov == nv] ++ [0] | nv <- b])

sameDegree :: Monomial -> Monomial -> Bool
sameDegree (ca, ea) (cb, eb) = ea == eb

addMatching :: Monomial -> [Monomial] -> Monomial
(ca, ea) `addMatching` [] = (ca, ea)
(ca, ea) `addMatching` ((cb, eb) : ms)
  | sameDegree (ca, ea) (cb, eb) = (ca + cb, ea) `addMatching` ms
  | otherwise = (ca, ea) `addMatching` ms

add :: Polynomial -> Polynomial -> Polynomial
(va, ma) `add` (vb, mb)
  | va /= vb = convertBasis (va, ma) b `add` convertBasis (vb, mb) b
  | otherwise = (b, ma `addMonomials` mb)
  where
    b = va `union` vb
    [] `addMonomials` m = m
    m `addMonomials` [] = m
    (ma : mas) `addMonomials` mb = (ma `addMatching` mb) : mas `addMonomials` filter (not . sameDegree ma) mb

multiplyMonomials :: Monomial -> Monomial -> Monomial
multiplyMonomials (ca, ea) (cb, eb) = (ca * cb, zipWith (+) ea eb)

multiply :: Polynomial -> Polynomial -> Polynomial
(va, ma) `multiply` (vb, mb)
  | va /= vb = convertBasis (va, ma) b `multiply` convertBasis (vb, mb) b
  | otherwise = (b, [a `multiplyMonomials` b | a <- ma, b <- mb])
  where
    b = va `union` vb

normalize :: Polynomial -> Polynomial
normalize (v, m) = (nv, sortBy sortMonomials nm)
  where
    (nv, nm) = convertBasis (v, normalizeMonomials m) $ sort v
    normalizeMonomials [] = []
    normalizeMonomials ((0, _) : ms) = normalizeMonomials ms
    normalizeMonomials (m : ms) = addMatching m ms : normalizeMonomials (filter (not . sameDegree m) ms)
    sortMonomials (ca, ea) (cb, eb) = compare eb ea

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

readPolynomial :: String -> Polynomial
readPolynomial s = (vars, readMonomials s')
  where
    readMonomials :: String -> [Monomial]
    readMonomials [] = []
    readMonomials m = (readCoefficient c, foldr (zipWith (+)) [0 | x <- vars] v) : readMonomials t
      where
        (c, r) = span (\x -> isDigit x || x == '+' || x == '-') m
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

main =
  do
    putStr "Input a polynomial: "
    input <- getLine
    let a = readPolynomial input
    putStrLn "You inputted the polynomial:"
    putStr $ showPolynomial a
    putStr " = "
    let norm_a = normalize a
    putStrLn $ showPolynomial norm_a

    putStr "Input another polynomial: "
    input <- getLine
    let b = readPolynomial input
    putStrLn "You inputted the polynomial:"
    putStr $ showPolynomial b
    putStr " = "
    let norm_b = normalize b
    putStrLn $ showPolynomial norm_b

    let sum = add a b
    putStrLn "Their sum:"
    putStr $ showPolynomial sum
    putStr " = "
    let norm_sum = normalize sum
    putStrLn $ showPolynomial norm_sum

    let product = multiply a b
    putStrLn "Their product:"
    putStr $ showPolynomial product
    putStr " = "
    let norm_product = normalize product
    putStrLn $ showPolynomial norm_product
