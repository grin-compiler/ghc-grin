module Main(main) where

-- hbcc prelude:
#ifdef __HBCC__
#include "prel.hs"
#endif

--import QSort
--module Sort(sortLe) where

sortLe :: (a -> a -> Bool) -> [a] -> [a]
sortLe le l = tmsort le l

--sort :: (Ord a) => [a] -> [a]
--sort l = tmsort (<=) l

tmsort _ [] = []
tmsort _ [x] = [x]              -- just for speed
tmsort le (x:xs) = msort le (upSeq le xs [x])

upSeq _  [] xs = [reverse xs]
upSeq le (y:ys) xxs@(x:xs) =
        if le x y then
            upSeq le ys (y:xxs)
        else
            reverse xxs : upSeq le ys [y]

msort _ [xs] = xs
msort le xss = msort le (mergePairs le xss)

mergePairs le (xs:ys:xss) = merge le xs ys : mergePairs le xss
mergePairs _  xss = xss

merge le xxs@(x:xs) yys = case yys of
                              [] -> xxs
                              (y:ys) -> if le x y then
                                            x:merge le xs yys
                                        else
                                            y:merge le xxs ys
merge _  []         yys = yys

--import List((\\))
-- hlib1.3/List.hs:
-- list difference (non-associative).  In the result of xs \\ ys,
-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  This (xs ++ ys) \\ xs == ys.
(\\)                    :: (Eq a) => [a] -> [a] -> [a]
bs \\ cs                =  flt bs cs
                           where [] `del` _         = []
                                 (x:xs) `del` y     = if x == y then xs else x : xs `del` y
--                                      | x == y    = xs
--                                      | otherwise = x : xs `del` y
                                 flt z []     =  z
                                 flt z (x:xs) =  flt (del z x) xs

-- Generate all possible permutations of length m from a list of scores
perms m [] = []
perms 1 l  = map (: []) l
perms m (n:ns) = map ((:) n) (perms (m-1) ns) ++ perms m ns

-- Find the (sorted) list of possible awards for a list of scores
awards :: [Int] -> [(String,(Int,[Int]))]
awards scores =
        award ("Gold",70) ++ award ("Silver",60) ++ award ("Bronze",50)
        where sumscores = map (\ p -> (sum p, p)) (perms 3 scores)
              atleast :: Int -> [(Int,[Int])]
              atleast threshold = filter foo sumscores
                                  where foo :: (Int,[Int]) -> Bool
                                        foo (sum,p) = sum >= threshold
              award (name,threshold) = map (\ ps -> (name,ps)) (sortLe le (atleast threshold))
              le :: (Int,[Int]) -> (Int,[Int]) -> Bool
              le (x,_) (y,_) = x <= y

-- Find all possible awards for a list of scores, counting each score once only
findawards :: [Int] -> [(String,(Int,[Int]))]
findawards scores | null theawards = []
                  | otherwise = firstaward : findawards (scores \\ perm)
        where firstaward@(award,(sum,perm)) = head theawards
              theawards = awards scores

-- Find the awards for all competitors, each competitor is a pair of
-- (Name, list of scores)
findallawards :: [(String,[Int])] -> [(String,[(String,(Int,[Int]))])]
findallawards competitors =
        map (\ (name,scores) -> (name,findawards scores)) competitors


competitors = [
                ("Simon",[35,27,40,19,34,21]),
                ("Hans",[23,19,45,17,10,5,8,14]),
                ("Phil",[1,18,35,20,21,19,34,8,16,21]),
                ("Kevin",[9,23,17,54,18,41,9,18,14])
              ]


main = print (findallawards competitors)
