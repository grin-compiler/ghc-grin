{-# LANGUAGE CPP, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Passes.DList
  ( Doms, domEntry, domLattice
  , domPass
  )
where

import Compiler.Hoopl


type Doms = WithBot [Label]
-- ^ List of labels, extended with a standard bottom element

-- | The fact that goes into the entry of a dominator analysis: the first node
-- is dominated only by the entry point, which is represented by the empty list
-- of labels.
domEntry :: Doms
domEntry = PElem []

domLattice :: DataflowLattice Doms
domLattice = addPoints "dominators" extend

extend :: JoinFun [Label]
extend _ (OldFact l) (NewFact l') = (changeIf (l `lengthDiffers` j), j)
    where j = lcs l l'
          lcs :: [Label] -> [Label] -> [Label] -- longest common suffix
          lcs l l' | length l > length l' = lcs (drop (length l - length l') l) l'
                   | length l < length l' = lcs l' l
                   | otherwise = dropUnlike l l' l
          dropUnlike [] [] maybe_like = maybe_like
          dropUnlike (x:xs) (y:ys) maybe_like =
              dropUnlike xs ys (if x == y then maybe_like else xs)
          dropUnlike _ _ _ = error "this can't happen"

          lengthDiffers [] [] = False
          lengthDiffers (_:xs) (_:ys) = lengthDiffers xs ys
          lengthDiffers [] (_:_) = True
          lengthDiffers (_:_) [] = True

-- | Dominator pass
domPass :: (NonLocal n, Monad m) => FwdPass m n Doms
domPass = FwdPass domLattice (mkFTransfer3 first (const id) distributeFact) noFwdRewrite
  where first n = fmap (entryLabel n:)
