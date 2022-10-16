{-# LANGUAGE FlexibleInstances #-}

module Polynomial where

import Classes (Normalizable (..))
import Monomial (Monomial (..), (~/=), (~=))

type Polynomial = [Monomial]

instance Num Polynomial where
  [] + p = p
  p + [] = p
  (ma : pa) + pb = (ma <+> pb) : (pa + filter (~/= ma) pb)

  pa * pb = [a * b | a <- pa, b <- pb]

  abs = map abs

  signum = map signum

  fromInteger i = [fromInteger i]

  negate = map negate

instance Normalizable Polynomial where
  normalize [] = []
  normalize (0 :^: _ : ms) = normalize ms
  normalize (m : ms) = m <+> ms : normalize (filter (~/= m) ms)

(<+>) :: Monomial -> Polynomial -> Monomial
m <+> [] = m
ma <+> (mb : ms)
  | ma ~= mb = (ma + mb) <+> ms
  | otherwise = ma <+> ms
