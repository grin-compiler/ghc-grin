{-# INLINE error #-};
error :: String -> a;
error s = _error s;

{-# INLINE chr, ord #-};
ord			:: Char -> Int;
chr 			:: Int -> Char;
ord c			=  Int# (ord# c);
chr (Int# i)		=  chr# i;
fromEnum n = ord n;

{-# INLINE head #-};
head			:: [a] -> a;
head (x:_)		=  x;
head []		=  error "head";

{-# INLINE tail #-};
tail			:: [a] -> [a];
tail (_:xs)		=  xs;
tail []		=  error "tail";

{-# INLINE map #-};
map :: (a -> b) -> [a] -> [b];
map f = map'
    where { map' []     = [];
            map' (x:xs) = f x : map' xs};

{-# INLINE (++) #-};
(++)			:: [a] -> [a] -> [a];
(++) = app
    where {
        app [] ys		=  ys;
        app (x:xs) ys	=  x : (app xs ys);
    };

{-# INLINE foldr #-};
foldr :: (a -> b -> b) -> b -> [a] -> b;
foldr f = foldr'
    where { foldr' z []      =  z;
	       foldr' z (x:xs) = f x (foldr' z xs)};

{-# INLINE concat #-};
concat			:: [[a]] -> [a];
concat = conc
  where {
    conc []		= [];
    conc ([]:xss)		= conc xss;
    conc (xs:xss)		= xs ++ conc xss};

#define putStr xprint
