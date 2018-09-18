{-# INLINE error #-};
error :: String -> a;
error s = _error s;

{-# INLINE head #-};
head			:: [a] -> a;
head (x:_)		=  x;
head []		=  error "head";

{-# INLINE tail #-};
tail			:: [a] -> [a];
tail (_:xs)		=  xs;
tail []		=  error "tail";

{-# INLINE null #-};
null			:: [a] -> Bool;
null []		=  True;
null (_:_)		=  False;

{-# INLINE length #-};
length			:: [a] -> Int;
length l		=  len l 0
	where { len :: [a]->Int->Int;
	        len []     a = a;
	        len (_:xs) a = len xs (a+1)};

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


{-# INLINE (++) #-};
(++)			:: [a] -> [a] -> [a];
(++) = app
    where {
        app [] ys		=  ys;
        app (x:xs) ys	=  x : (app xs ys);};

#define putStr xprint
