{-# INLINE error #-}
error :: String -> a
error s = _error s

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

{-# INLINE (++) #-}
(++)			:: [a] -> [a] -> [a]
(++) = app
    where app []     ys	=  ys
          app (x:xs) ys	=  x : (app xs ys)

{-# INLINE head #-}
head :: [a] -> a
head (x:_) =  x
head []	   = error "head"

{-# INLINE null #-}
null :: [a] -> Bool
null []	   =  True
null (_:_) =  False

{-# INLINE reverse #-}
reverse		:: [a] -> [a]
reverse l	=  rev l []
	where rev []     a = a
	      rev (x:xs) a = rev xs (x:a)

--{-# SPECIALIZE sum :: [Int] -> Int #-}
--sum		:: (Num a) => [a] -> a
-- hbcc bug!
{-# INLINE sum #-}
sum :: [Int] -> Int
sum	l	= sum' l 0
	where sum' :: [Int] -> Int -> Int
	      sum' []     a = a
	      sum' (x:xs) a = sum' xs (a+x)

#define print xprint
