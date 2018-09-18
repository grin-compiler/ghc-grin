-- $Id: prel.hs,v 1.3 1999/02/07 20:28:10 boquist Exp $

-- repeat x is an infinite list, with x the value of every element.
repeat			:: a -> [a]
--repeat x		=  xs where xs = x:xs
repeat x = x : repeat x -- XXX no letrec

{-# SPECIALIZE elem :: Char -> [Char] -> Bool #-}
elem		:: (Eq a) => a -> [a] -> Bool
elem _ []	= False
elem x (y:ys)	= x==y || elem x ys

{-# INLINE map #-}
map :: (a -> b) -> [a] -> [b]
map f = map'
    where map' []     = []
          map' (x:xs) = f x : map' xs

{-# INLINE concatMap #-}
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concatMap'
    where concatMap' []     = []
          concatMap' (x:xs) = f x ++ concatMap' xs

{-# INLINE filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter f = filter'
    where filter' []     = []
          filter' (x:xs) = if f x then
                               x : filter' xs
                           else
                               filter' xs

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f = foldr'
    where foldr' z []      =  z
	  foldr' z (x:xs) = f x (foldr' z xs)

{-# INLINE take #-}
take			:: Int -> [b] -> [b]
take = itake
	where itake :: Int -> [b] -> [b]
	      itake  0     _		=  []
	      itake  _     []		=  []
	      itake  n     (x:xs)	=  x : itake (n-1) xs

{-# INLINE error #-}
error :: String -> a
error s = _error s

-- list concatenation (right-associative)
{-# INLINE (++) #-}
(++)			:: [a] -> [a] -> [a]
(++) = app
    where app []     ys	=  ys
          app (x:xs) ys	=  x : (app xs ys)

-- concat, applied to a list of lists, returns their flattened concatenation.
{-# INLINE concat #-}
concat			:: [[a]] -> [a]
concat = conc
  where
    conc []		= []
    conc ([]:xss)		= conc xss			-- for better stack behaviour!
    conc (xs:xss)		= xs ++ conc xss

#define putStr xprint
