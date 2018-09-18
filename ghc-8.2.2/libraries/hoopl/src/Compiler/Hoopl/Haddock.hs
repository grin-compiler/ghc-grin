{-# LANGUAGE CPP, GADTs #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Haddock
where

data Lit a where
  I :: Int  -> Lit Int  -- ^ an integer
  B :: Bool -> Lit Bool -- ^ a Boolean

