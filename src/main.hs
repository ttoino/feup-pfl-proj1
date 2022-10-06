import Data.Char (isLetter)
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

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = splitBy (== x)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f [] = []
splitBy f (x : xs) | f x = splitBy f xs
splitBy f a = p : splitBy f a'
  where
    (p, a') = break f a

readPolynomial :: String -> Polynomial
readPolynomial s = (vars, map readMonomial (splitOn '+' s))
  where
    readMonomial m = (read h, readVars vars (map (splitBy (\x -> x == '^' || x == ' ')) t))
      where
        (h : t) = splitOn '*' m
    vars = nub $ sort $ filter isLetter s
    readVars [] a = []
    readVars (v : vs) o = readVarsHelper v o : readVars vs o
    readVarsHelper v [] = 0
    readVarsHelper v ([var, e] : o) | head var == v = read e
    readVarsHelper v ([var] : o) | head var == v = 1
    readVarsHelper v (x : xs) = readVarsHelper v xs

a :: Polynomial
a = ("x", [(7, [3]), (5, [1]), (1, [0])])

b :: Polynomial
b =
  ( "xyz",
    [ (0, [2, 0, 0]),
      (2, [0, 1, 0]),
      (5, [0, 0, 1]),
      (1, [0, 1, 0]),
      (7, [0, 2, 0])
    ]
  )

main =
  do
    putStrLn $ "a = " ++ showPolynomial (normalize a)
    putStrLn $ "b = " ++ showPolynomial (normalize b)

    putStrLn $ "a + a = " ++ showPolynomial (normalize $ add a a)
    putStrLn $ "b + b = " ++ showPolynomial (normalize $ add b b)
    putStrLn $ "b + a = " ++ showPolynomial (normalize $ add b a)

    putStrLn $ "a * a = " ++ showPolynomial (normalize $ multiply a a)
    putStrLn $ "b * b = " ++ showPolynomial (normalize $ multiply b b)
    putStrLn $ "b * a = " ++ showPolynomial (normalize $ multiply b a)
