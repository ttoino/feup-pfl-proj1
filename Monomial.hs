module Monomial where

import Util (showReal, showSubscript, showSuperscript)

type Exponent = Integer

type Coefficient = Double

data Monomial = Coefficient :^: [Exponent]
  deriving (Eq)

instance Show Monomial where
  show (c :^: e)
    | c == 1 && any (> 0) e = showVars e
    | c == (-1) && any (> 0) e = '-' : showVars e
    | otherwise = showReal c ++ showVars e
    where
      showVars e = concat (zipWith showVar [1 ..] e)
      showVar v 0 = []
      showVar v 1 = 'x' : showSubscript v
      showVar v e = 'x' : (showSubscript v ++ showSuperscript e)

  showList [] = ("" ++)
  showList [m] = (show m ++)
  showList (m : (c :^: e) : ms) = ((show m ++ showSign c ++ show ((abs c :^: e) : ms)) ++)
    where
      showSign c = if c < 0 then " - " else " + "

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

(~=) :: Monomial -> Monomial -> Bool
(ca :^: ea) ~= (cb :^: eb) = ea == eb

(~/=) :: Monomial -> Monomial -> Bool
(~/=) a = not . (a ~=)
