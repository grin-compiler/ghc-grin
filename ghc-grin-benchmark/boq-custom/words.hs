-- $Id: words.hs,v 1.3 1999/02/09 00:41:29 boquist Exp $

module Main(main) where

import RTS

--main = print (alllookswedish "annika")
main = print_int_list (map count "abcdefghijklmnopqrstuvwxyz")
       where chars :: String
             chars = concat (concatMap alllookswedish input)
             input = ["annika","urban","gustav","janne","thomas","lennart",
                      "magnus","lars","niklas","john","bengt","christer"]
             count :: Char -> Int
             count c = count' 0 chars
                       where count' :: Int -> String -> Int
                             count' n [] = n
                             count' n (x:xs) = if c == x then
                                                   count' (n+1) xs
                                               else
                                                   count' n xs

-- import Subsequences(permutations)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [ zs | ys <- permutations xs, zs <- insertAll x ys ]
        where insertAll x [] = [[x]]
              insertAll x yys@(y:ys) = (x:yys) : map (y:) (insertAll x ys)

-------------------------------------------------------------------------------
konsonant :: Char -> Bool
konsonant c = c `elem` "qwrtpsdfghjklzxcvbnm"

vokal :: Char -> Bool
vokal c = c `elem` "aouåeiyäö"

-- True om alla tecken är konsonanter:
allkonsonant :: String -> Bool
allkonsonant ws = all konsonant ws

-- True om det inte finns 4 konsonanter i rad:
no4konsinrow :: String -> Bool
no4konsinrow ws = length ws < 4 ||
                  let (four,rest) = splitAt 4 ws
                  in
                  not (allkonsonant four) && no4konsinrow (tail ws)

-- True om det inte finns två vokaler i rad:
no2vokalinrow :: String -> Bool
no2vokalinrow []       = True
no2vokalinrow [c]      = True
no2vokalinrow (c:h:cs) = not (vokal c && vokal h) && no2vokalinrow (h:cs)

-- True om det finns en vokal i teckenlistan:
containvokal :: String -> Bool
containvokal cs = any vokal cs

startcheck :: String -> Bool
startcheck word = length word < 4 ||
                  case word of
                      ('s':'t':'r':w) -> True
                      ('s':'k':'r':w) -> True
                      ('s':'k':'j':w) -> True
                      ('s':'t':'j':w) -> True
                      ('s':'c':'h':w) -> True
                      w               -> not (allkonsonant (fst (splitAt 3 w)))

-- ger de n sista elementen:
takelast :: Int -> [a] -> [a]
takelast n xs = if length xs < n
                then error "takelast"
                else drop (length xs - n) xs

endcheck :: String -> Bool
endcheck ws = length ws < 3 ||
              let (slut@[c,h,k]) = takelast 3 ws
              in not (allkonsonant slut) || c == h

swedishlook :: String -> Bool
swedishlook wordstr = no4konsinrow wordstr  &&
                      no2vokalinrow wordstr &&
                      containvokal wordstr  &&
                      startcheck wordstr    &&
                      endcheck wordstr

alllookswedish :: String -> [String]
alllookswedish word = filter swedishlook (permutations word)
