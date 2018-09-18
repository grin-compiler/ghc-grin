{-# INLINE error #-}
error :: String -> a
error s = _error s

{-# INLINE map #-}
map :: (a -> b) -> [a] -> [b]
map f = map'
    where map' []     = []
          map' (x:xs) = f x : map' xs

{-# INLINE head #-}
head			:: [a] -> a
head (x:_)		=  x
head []		=  error "head"

{-# INLINE last #-}
last			:: [a] -> a
last l = last' l
    where last'			:: [a] -> a
	  last' [] = error "last"
	  last' (x:xs) = case xs of
			     []  -> x
			     _:_ -> last' xs

{-# INLINE length #-}
length			:: [a] -> Int
length l		=  len l 0
	where len :: [a]->Int->Int
	      len []     a = a
	      len (_:xs) a = len xs (a+1)

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f = foldr'
    where foldr' z []      =  z
	  foldr' z (x:xs) = f x (foldr' z xs)

{-# INLINE (++) #-}
(++)			:: [a] -> [a] -> [a]
(++) = app
    where app []     ys	=  ys
          app (x:xs) ys	=  x : (app xs ys)

{-# INLINE concat #-}
concat			:: [[a]] -> [a]
concat = conc
  where
    conc []		= []
    conc ([]:xss)		= conc xss			-- for better stack behaviour!
    conc (xs:xss)		= xs ++ conc xss

{-# INLINE all #-}
all		:: (a -> Bool) -> [a] -> Bool
all p = all'
    where all' []	=  True
	  all' (x:xs)	=  p x && all' xs

#define print xprint
