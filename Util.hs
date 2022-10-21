module Util where

import Data.Text (pack, replace, unpack)

-- | Returns the digits of a number
digits :: Integral a => a -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [fromIntegral (n `mod` 10)]

-- | Converts an integral to it's superscripted representation
showSuperscript :: Integral a => a -> String
showSuperscript e = map (supers !!) (digits e)
  where
    supers = "⁰¹²³⁴⁵⁶⁷⁸⁹"

-- | Converts an integral to it's subscripted representation
showSubscript :: Integral a => a -> String
showSubscript e = map (subs !!) (digits e)
  where
    subs = "₀₁₂₃₄₅₆₇₈₉"

-- | Shows a number
showReal :: (Real a, Show a) => a -> String
showReal c
  | dec == ".0" = int
  | otherwise = s
  where
    s = show c
    (int, dec) = span (/= '.') s
