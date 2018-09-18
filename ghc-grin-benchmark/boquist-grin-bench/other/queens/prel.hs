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

-- list concatenation (right-associative)
{-# INLINE (++) #-}
(++)			:: [a] -> [a] -> [a]
(++) = app
    where app []     ys	=  ys
          app (x:xs) ys	=  x : (app xs ys)

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

#define print xprint
