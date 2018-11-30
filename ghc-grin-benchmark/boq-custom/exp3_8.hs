module Main(main) where

import RTS

infix 8 ^^^

data Nat = Z | S Nat -- deriving (Eq,Ord,Show{-was:Text-})

--instance Num Nat where
add :: Nat -> Nat -> Nat
add Z     y = y
add (S x) y = S (x `add` y)

mul :: Nat -> Nat -> Nat
mul x Z     = Z
mul x (S y) = (x `mul` y) `add` x

toNat :: Int -> Nat
toNat x = if x < 1 then Z else S (toNat (x-1))

-- partain:sig
int :: Nat -> Int
int Z     = 0
int (S x) = 1 + int x


(^^^) :: Nat -> Nat -> Nat
x ^^^ Z   = S Z
x ^^^ S y = x `mul` (x ^^^ y)

main = print_int (int (toNat 3 ^^^ toNat 8))
