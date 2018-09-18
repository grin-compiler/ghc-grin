{-# INLINE (++) #-};
(++) :: [a] -> [a] -> [a];
(++) = app
    where {
        app [] ys     = ys;
        app (x:xs) ys = x : (app xs ys)};

{-# INLINE length #-};
length			:: [a] -> Int;
length l		=  len l 0
	where {
	    len :: [a]->Int->Int;
	    len []     a = a;
	    len (_:xs) a = len xs (a+1)};

{-# INLINE error #-};
error :: String -> a;
error s = _error s;

{-# INLINE (!!) #-};
(!!) :: [a] -> Int -> a;
(!!) = listInd
    where {
	listInd (x:_)  0 = x;
	listInd (_:xs) n = listInd xs (n-1);
	listInd []     _ = error "listInd"};

{-# INLINE map #-};
map :: (a -> b) -> [a] -> [b];
map f = map'
    where { map' []     = [];
            map' (x:xs) = f x : map' xs};

{-# INLINE foldl #-};
foldl :: (a -> b -> a) -> a -> [b] -> a;
foldl f = loop
    where {
        loop z [] = z;
	loop z (x:xs) =  loop (f z x) xs};

{-# INLINE concat #-};
concat			:: [[a]] -> [a];
concat = conc
  where {
    conc []		= [];
    conc ([]:xss)		= conc xss;			-- for better stack behaviour!
    conc (xs:xss)		= xs ++ conc xss};

#define putStr xprint
