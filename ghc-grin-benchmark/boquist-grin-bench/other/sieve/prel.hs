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
