{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Compiler.Hoopl.Stream
where

import Control.Monad

import Test.QuickCheck

type Stream s a = s -> Maybe (Pair s a)
data Pair s a = Pair a (Stream s a)

instance Show (s -> Maybe (Pair s a)) where
  show _ = "<function>"

instance (Arbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Pair s a) where
  arbitrary = liftM2 Pair arbitrary arbitrary
  shrink (Pair a f) = [Pair a' f' | a' <- shrink a, f' <- shrink f] 

emptyS :: Stream s a
emptyS = const Nothing

thenS :: Stream s a -> Stream s a -> Stream s a
s1 `thenS` s2 = \s -> case s1 s of
                        Nothing -> s2 s
                        Just (Pair a s1') -> Just $ Pair a (s1' `thenS` s2)

iterS :: Stream s a -> Stream s a
iterS stream = \s -> case stream s of
                       Nothing -> Nothing
                       Just (Pair a s') -> Just $ Pair a (s' `thenS` iterS stream)

elems :: s -> Stream s a -> [a]
elems s f = case f s of Nothing -> []
                        Just (Pair a f) -> a : elems s f

law1 :: Eq a => Int -> s -> Stream s a -> Bool
law1 n sigma stream = iterS stream `eq` (stream `thenS` iterS stream)
  where s `eq` s' = take n (elems sigma s) == take n (elems sigma s')

law2 :: Bool
law2 = iterS emptyS `eq` (emptyS :: Stream () Int)
  where s `eq` s' = elems () s == elems () s'

----------------------------------------------------------------

-- list analogy

emptyL :: [a]
emptyL = []

thenL :: [a] -> [a] -> [a]
thenL = (++)

iterL :: [a] -> [a]
iterL [] = []
iterL (x:xs) = x : (xs `thenL` iterL (x:xs))

law1' :: Eq a => Int -> [a] -> Bool
law1' n l = iterL l `eq` (l `thenL` iterL l)
 where xs `eq` ys = take n xs == take n ys

law2' :: Bool
law2' = iterL emptyL == (emptyL :: [Int])
