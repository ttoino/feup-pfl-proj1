{-# LANGUAGE MultiParamTypeClasses #-}

module Classes where

class Derivable a b where
  (//) :: a -> b -> a

class Normalizable a where
  normalize :: a -> a
