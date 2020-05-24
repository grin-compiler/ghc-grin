module Stg.UnariseType (unariseType, unariseTypePrimRep) where

import GHC.Prelude

import Data.Maybe
import Data.Semigroup
import Data.List (sort, foldl')
import GHC.Types.RepType
import BasicTypes
import TysWiredIn
import Type
import Util

type SortedSlotTys = [Arg SlotTy [PrimRep]]

-- INVARIANT: Result slots are sorted (via Ord SlotTy), except that at the head
-- of the list we have the slot for the tag.
ubxSumRepTypeWithReps :: [[PrimRep]] -> [Arg SlotTy [PrimRep]]
ubxSumRepTypeWithReps constrs0
  -- These first two cases never classify an actual unboxed sum, which always
  -- has at least two disjuncts. But it could happen if a user writes, e.g.,
  -- forall (a :: TYPE (SumRep [IntRep])). ...
  -- which could never be instantiated. We still don't want to panic.
{-
  | constrs0 `lengthLessThan` 2
  = [Arg WordSlot [WordRep]]
-}
  | otherwise
  = let
      combine_alts :: [SortedSlotTys]  -- slots of constructors
                   -> SortedSlotTys    -- final slots
      combine_alts constrs = foldl' merge [] constrs

      merge :: SortedSlotTys -> SortedSlotTys -> SortedSlotTys
      merge existing_slots []
        = existing_slots
      merge [] needed_slots
        = needed_slots
      merge (es@(Arg esST esRT) : ess) (s@(Arg sST sRT) : ss)
        | Just s' <- sST `fitsIn` esST
        = -- found a slot, use it
          Arg s' (esRT ++ sRT) : merge ess ss
        | s < es
        = -- we need a new slot and this is the right place for it
          s : merge (es : ess) ss
        | otherwise
        = -- keep searching for a slot
          es : merge ess (s : ss)

      -- Nesting unboxed tuples and sums is OK, so we need to flatten first.
      rep :: [PrimRep] -> SortedSlotTys
      rep ty = sort [Arg (primRepSlot t) [t] | t <- ty]

      sumRep = Arg WordSlot [WordRep] : combine_alts (map rep constrs0)
               -- WordSlot: for the tag of the sum
    in
      sumRep
{-
-- | Returns the bigger type if one fits into the other. (commutative)
fitsIn :: SlotTy -> SlotTy -> Maybe SlotTy
fitsIn ty1 ty2
  | isWordSlot ty1 && isWordSlot ty2
  = Just (max ty1 ty2)
  | isFloatSlot ty1 && isFloatSlot ty2
  = Just (max ty1 ty2)
  | isPtrSlot ty1 && isPtrSlot ty2
  = Just PtrSlot
  | otherwise
  = Nothing
  where
    isPtrSlot PtrSlot = True
    isPtrSlot _       = False

    isWordSlot Word64Slot = True
    isWordSlot WordSlot   = True
    isWordSlot _          = False

    isFloatSlot DoubleSlot = True
    isFloatSlot FloatSlot  = True
    isFloatSlot _          = False
-}
joinSortedSlotTys :: SortedSlotTys -> [PrimRep]
joinSortedSlotTys = map joinReps where
  {-
  isLiftedRep LiftedRep = True
  isLiftedRep _         = False
  joinReps (Arg PtrSlot reps) = if any isLiftedRep reps then LiftedRep else UnliftedRep
  joinReps (Arg st _)         = slotPrimRep st
  -}
  joinReps (Arg st l) = head l

unariseTypePrimRep :: Type -> [PrimRep]
unariseTypePrimRep t
  | isUnboxedSumType t
  = joinSortedSlotTys . ubxSumRepTypeWithReps . map unariseTypePrimRep . dropRuntimeRepArgs $ fromMaybe [] (tyConAppArgs_maybe t)

  | otherwise
  = typePrimRep t

unariseType :: Type -> Type
unariseType t
  | isTypeLevPoly t      = t
  | isUnboxedSumType t   = mkTupleTy Unboxed . map primRepToType . unariseTypePrimRep $ t
  | otherwise            = t
