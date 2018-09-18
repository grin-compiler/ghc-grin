{-# INLINE error #-}
error :: String -> a
error s = _error s

{-# INLINE length #-}
length			:: [a] -> Int
length l		=  len l 0
	where len :: [a]->Int->Int
	      len []     a = a
	      len (_:xs) a = len xs (a+1)

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

{-# INLINE (++) #-}
(++)			:: [a] -> [a] -> [a]
(++) = app
    where app []     ys	=  ys
          app (x:xs) ys	=  x : (app xs ys)

{-# INLINE all #-}
all		:: (a -> Bool) -> [a] -> Bool
all p = all'
    where all' []	=  True
	  all' (x:xs)	=  p x && all' xs

{-# INLINE map #-}
map :: (a -> b) -> [a] -> [b]
map f = map'
    where map' []     = []
          map' (x:xs) = f x : map' xs

#define print xprint
