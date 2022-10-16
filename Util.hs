module Util where

import Data.Text (pack, replace, unpack)

digits :: Integral a => a -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [fromIntegral (n `mod` 10)]

showSuperscript :: Integral a => a -> String
showSuperscript e = map (supers !!) (digits e)
  where
    supers = "⁰¹²³⁴⁵⁶⁷⁸⁹"

showSubscript :: Integral a => a -> String
showSubscript e = map (subs !!) (digits e)
  where
    subs = "₀₁₂₃₄₅₆₇₈₉"

showReal :: (Real a, Show a) => a -> String
showReal c
  | dec == ".0" = int
  | otherwise = s
  where
    s = show c
    (int, dec) = span (/= '.') s

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

replaceS :: String -> String -> String -> String
replaceS a b s = unpack $ replace (pack a) (pack b) (pack s)
