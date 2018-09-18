{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f = foldr'
    where foldr' z []      =  z
	  foldr' z (x:xs) = f x (foldr' z xs)

--{-# SPECIALIZE sum :: [Int] -> Int #-}
--sum		:: (Num a) => [a] -> a
-- hbcc bug!
{-# INLINE sum #-}
sum :: [Int] -> Int
sum	l	= sum' l 0
	where sum' :: [Int] -> Int -> Int
	      sum' []     a = a
	      sum' (x:xs) a = sum' xs (a+x)

{-# INLINE (++) #-}
(++)			:: [a] -> [a] -> [a]
(++) = app
    where app []     ys	=  ys
          app (x:xs) ys	=  x : (app xs ys)

{-# INLINE reverse #-}
reverse		:: [a] -> [a]
reverse l	=  rev l []
	where rev []     a = a
	      rev (x:xs) a = rev xs (x:a)

#define print xprint
