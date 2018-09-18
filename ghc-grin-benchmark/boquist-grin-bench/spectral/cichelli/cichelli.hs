module Main where

-- hbcc prelude:
#ifdef __HBCC__
#include "prel.hs"
#endif

keys :: [String]
--keys = ["case","class","data","default","deriving","else","hiding", "if","import","in","infix","infixl","instance","interface","let","module","of","renaming","then","to","type","where"]
--keys = ["expect","respect","conduct","viaduct","tender","render","sender","cinder","vender","ride","confide","tried","visit","restrict","stoic","historic"]
keys = ["john","jane","janet","kate","katy","tony","bert","betty","cathy","carol","timmy","judy","ivy","ian","jan","jack","eva","ivan","linda","andy","edna"]

--module Aux where

--import Key

data Key = K String Char Char Int {- String, end letters, length of string -}
data HashSet = H (Maybe Int) (Maybe Int) [Int]
type HashFun = [(Char,Int)]  {- Association list of Character to values -}
--1.3:data Maybe a = Nothing | Just a deriving Text

ends :: Key -> String
ends (K _ a z _) = [a,z]

morefreq :: Key -> Key -> Bool
morefreq (K _ a x _) (K _ b y _) = freq a + freq x > freq b + freq y

freq :: Char -> Int
freq c = assoc c freqtab

assoc :: (Eq a) => a -> [(a,b)] -> b
assoc x ((y,z):yzs) = if x == y then z else assoc x yzs

assocm :: (Eq a) => a -> [(a,b)] -> Maybe b
assocm x [] = Nothing
assocm x ((y,z):yzs) = if x == y then Just z else assocm x yzs

freqtab :: [(Char, Int)]
freqtab = histo (concat (map ends attribkeys))

histo :: (Eq a) => [a] -> [(a,Int)]
histo = foldr histins []
        where
        histins x [] = [(x,1)]
        histins x (yn@(y,n):yns) = if x==y then (y,n+1):yns
                                   else yn:histins x yns

maxval :: Int
maxval = length (freqtab)

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (\x -> member x ys) xs
 
--partain: in the prelude
--all :: (a->Bool) -> [a] -> Bool
--all p = foldr (\x -> \b ->(p x && b)) True
 
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (member y xs)]
 
attribkeys :: [Key]
attribkeys = map (\k->(K k (head k) (last k) (length k))) keys
 
hinsert :: Int -> HashSet -> Maybe HashSet
hinsert h (H lo hi hs) =
    if member h hs || 1 + hi'- lo' > numberofkeys then Nothing
    else Just (H (Just lo') (Just hi') (h:hs))
    where
    lo' = minm lo h
    hi' = maxm hi h
 
minm, maxm :: Maybe Int -> Int -> Int
minm Nothing y = y
minm (Just x) y = if x <= y then x else y
maxm Nothing y = y
maxm (Just x) y = if x > y then x else y
 
member :: (Eq a) => a -> [a] -> Bool
member _ [] = False
member x (y:ys) = x == y || member x ys
 
hash :: HashFun -> Key -> Int
hash cvs (K _ a z n) = n + assoc a cvs + assoc z cvs
 
numberofkeys :: Int
numberofkeys = length keys
 

partition' :: (a->Bool) -> [a] -> ([a],[a])
partition' p = foldr select ([],[])
              where select x (ts,fs) | p x       = (x:ts,fs)
                                     | otherwise = (ts,x:fs)

freqsorted :: [Key] -> [Key]
freqsorted =
	\x->x
    {-foldr freqins []
    where
    freqins x [] = [x]
    freqins x (y:ys) = if morefreq x y then x:y:ys else y:freqins x ys-}
 
blocked :: [Key] -> [Key]
blocked = blocked' []
blocked' ds [] = []
blocked' ds (k : ks) = k : det ++ blocked' ds' rest
                     where
                     (det,rest) = partition' (\x->subset (ends x) ds') ks
                     ds' = union ds (ends k)


data Status a = NotEver Int | YesIts Int a deriving ()

showit :: Status HashFun -> (String,(Int,[(Char,Int)]))
showit (NotEver i)  = ("NotEver", (i, []))
showit (YesIts i a) = ("YesIts ", (i, a))

type FeedBack = Status HashFun

cichelli :: FeedBack
cichelli = findhash hashkeys
                where
-- #ifdef SORTED
                hashkeys = (blocked.freqsorted) attribkeys
-- #else
--                hashkeys = blocked attribkeys
-- #endif

	
findhash :: [Key] -> FeedBack 
findhash = findhash' (H Nothing Nothing []) []


findhash' :: HashSet -> HashFun -> [Key] -> FeedBack
findhash' keyHashSet charAssocs [] = (YesIts 1 charAssocs)
findhash' keyHashSet charAssocs (k@(K s a z n):ks) =
  ( case (assocm a charAssocs, assocm z charAssocs) of
	  (Nothing,Nothing) -> if a==z then 
				firstSuccess (\m->try [(a,m)]) (upto 0 maxval) 
				else 
				firstSuccess (\(m,n)->try [(a,m),(z,n)]) 
					    [(m,n) | m<-(upto 0 maxval), n<-(upto 0 maxval)]
          (Nothing,Just zc) -> firstSuccess (\m->try [(a,m)]) (upto 0 maxval) 
	  (Just ac,Nothing) -> firstSuccess (\n->try [(z,n)]) (upto 0 maxval)
	  (Just ac,Just zc) -> try [] )
  where
  try newAssocs = ( case hinsert (hash newCharAssocs k) keyHashSet of
             Nothing -> (NotEver 1)
             Just newKeyHashSet -> findhash' newKeyHashSet newCharAssocs ks )
             where
             newCharAssocs = newAssocs ++ charAssocs
upto :: Int -> Int -> [Int]
upto m n = if m > n then [] else m : upto (m+1) n

-- Returns the first successful `working' function on a list of possible arguments
firstSuccess :: (a -> FeedBack) -> [a] -> FeedBack 
firstSuccess f possibles =  first 0 (map f possibles) 

first :: Int -> [FeedBack] -> FeedBack
first k [] = NotEver k
first k (a:l) = case a of
                (YesIts leaves y) -> YesIts (k+leaves) y
                (NotEver leaves)    -> first (k+leaves) l

main = print (showit cichelli)
