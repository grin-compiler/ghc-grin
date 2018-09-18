-- $Id: prel.hs,v 1.3 1999/02/09 00:41:29 boquist Exp $

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.
any		:: (a -> Bool) -> [a] -> Bool
any p []	= False
any p (x:xs)	= p x || any p xs
all		:: (a -> Bool) -> [a] -> Bool
all p []	=  True
all p (x:xs)	=  p x && all p xs

{-# SPECIALIZE elem :: Int -> [Int] -> Bool, Char -> [Char] -> Bool, String -> [String] -> Bool #-}
elem		:: (Eq a) => a -> [a] -> Bool
elem _ []	= False
elem x (y:ys)	= x==y || elem x ys

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

{-# INLINE concatMap #-}
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concatMap'
    where concatMap' []     = []
          concatMap' (x:xs) = f x ++ concatMap' xs

{-# INLINE map #-}
map :: (a -> b) -> [a] -> [b]
map f = map'
    where map' []     = []
          map' (x:xs) = f x : map' xs

{-# INLINE filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter f = filter'
    where filter' []     = []
          filter' (x:xs) = if f x then
                               x : filter' xs
                           else
                               filter' xs

{-# INLINE length #-}
length			:: [a] -> Int
length l		=  len l 0
	where len :: [a]->Int->Int
	      len []     a = a
	      len (_:xs) a = len xs (a+1)

{-# INLINE splitAt #-}
splitAt			:: Int -> [b] -> ([b],[b])
splitAt = isplitAt
	where isplitAt		:: Int -> [b] -> ([b],[b])
	      isplitAt  0     xs	=  ([],xs)
	      isplitAt  _     []	=  ([],[])
	      isplitAt  n     (x:xs)	=  (x:xs',xs'') where (xs',xs'') = isplitAt (n-1) xs

{-# INLINE tail #-}
tail			:: [a] -> [a]
tail (_:xs)		=  xs
tail []			=  error "tail"

{-# INLINE drop #-}
drop			:: Int -> [b] -> [b]
drop = idrop
	where idrop			:: Int -> [b] -> [b]
	      idrop  0     xs		=  xs
	      idrop  _     []		=  []
	      idrop  n     (_:xs)	=  idrop (n-1) xs

{-# INLINE fst #-}
fst			:: (a,b) -> a
fst (x,y)		=  x

{-# INLINE error #-}
error :: String -> a
error s = _error s

#define print xprint
