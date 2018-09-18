{-# LANGUAGE CPP, GADTs, RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module OptSupport (mapVE, mapEE, mapEN, mapVN, fold_EE, fold_EN, insnToG) where

import Control.Monad
import Data.Maybe
import Prelude hiding (succ)

import Control.Applicative as AP (Applicative(..))
import Compiler.Hoopl hiding ((<*>))
import IR

----------------------------------------------
-- Map/Fold functions for expressions/insns
----------------------------------------------

type Node = Insn
type MaybeChange a = a -> Maybe a
mapVE :: (Var  -> Maybe Expr) -> MaybeChange Expr
mapEE :: MaybeChange Expr     -> MaybeChange Expr
mapEN :: MaybeChange Expr     -> MaybeChange (Node e x)
mapVN :: (Var  -> Maybe Expr) -> MaybeChange (Node e x)

mapVN = mapEN . mapEE . mapVE

mapVE f (Var v) = f v
mapVE _ _       = Nothing


data Mapped a = Old a | New a

instance Monad Mapped where
  return = AP.pure

  Old a >>= k = k a
  New a >>= k = asNew (k a)
    where asNew (Old a)   = New a
          asNew m@(New _) = m

instance Functor Mapped where
  fmap = liftM

instance Applicative Mapped where
  pure = Old
  (<*>) = ap


makeTotal :: (a -> Maybe a) -> (a -> Mapped a)
makeTotal f a = case f a of Just a' -> New a'
                            Nothing -> Old a
makeTotalDefault :: b -> (a -> Maybe b) -> (a -> Mapped b)
makeTotalDefault b f a = case f a of Just b' -> New b'
                                     Nothing -> Old b
ifNew :: Mapped a -> Maybe a
ifNew (New a) = Just a
ifNew (Old _) = Nothing

type Mapping a b = a -> Mapped b

(/@/) :: Mapping b c -> Mapping a b -> Mapping a c
f /@/ g = \x -> g x >>= f


class HasExpressions a where
  mapAllSubexpressions :: Mapping Expr Expr -> Mapping a a

instance HasExpressions (Insn e x) where
  mapAllSubexpressions = error "urk!" (mapVars, (/@/), makeTotal, ifNew)

mapVars :: (Var -> Maybe Expr) -> Mapping Expr Expr
mapVars f e@(Var x) = makeTotalDefault e f x
mapVars _ e         = return e


mapEE f e@(Lit _)     = f e
mapEE f e@(Var _)     = f e
mapEE f e@(Load addr) =
  case mapEE f addr of
    Just addr' -> Just $ fromMaybe e' (f e')
                    where e' = Load addr'
    Nothing    -> f e
mapEE f e@(Binop op e1 e2) =
  case (mapEE f e1, mapEE f e2) of
    (Nothing, Nothing) -> f e
    (e1',     e2')     -> Just $ fromMaybe e' (f e')
                    where e' = Binop op (fromMaybe e1 e1') (fromMaybe e2 e2')

mapEN _   (Label _)           = Nothing
mapEN f   (Assign v e)        = liftM (Assign v) $ f e
mapEN f   (Store addr e)      =
  case (f addr, f e) of
    (Nothing, Nothing) -> Nothing
    (addr', e') -> Just $ Store (fromMaybe addr addr') (fromMaybe e e')
mapEN _   (Branch _)          = Nothing
mapEN f   (Cond e tid fid)    =
  case f e of Just e' -> Just $ Cond e' tid fid
              Nothing -> Nothing
mapEN f   (Call rs n es succ) =
  if all isNothing es' then Nothing
  else Just $ Call rs n (map (uncurry fromMaybe) (zip es es')) succ
    where es' = map f es
mapEN f   (Return es) =
   if all isNothing es' then Nothing
   else Just $ Return (map (uncurry fromMaybe) (zip es es'))
     where es' = map f es

fold_EE :: (a -> Expr -> a) -> a -> Expr      -> a
fold_EN :: (a -> Expr -> a) -> a -> Insn e x -> a

fold_EE f z e@(Lit _)         = f z e
fold_EE f z e@(Var _)         = f z e
fold_EE f z e@(Load addr)     = f (fold_EE f z addr) e
fold_EE f z e@(Binop _ e1 e2) =
  let afterE1 = fold_EE f z e1
      afterE2 = fold_EE f afterE1 e2
  in f afterE2 e

fold_EN _ z (Label _)       = z
fold_EN f z (Assign _ e)    = f z e
fold_EN f z (Store addr e)  = f (f z e) addr
fold_EN _ z (Branch _)      = z
fold_EN f z (Cond e _ _)    = f z e
fold_EN f z (Call _ _ es _) = foldl f z es
fold_EN f z (Return es)     = foldl f z es

----------------------------------------------
-- Lift a insn to a Graph
----------------------------------------------

insnToG :: Insn e x -> Graph Insn e x
insnToG n@(Label _)      = mkFirst n
insnToG n@(Assign _ _)   = mkMiddle n
insnToG n@(Store _ _)    = mkMiddle n
insnToG n@(Branch _)     = mkLast n
insnToG n@(Cond _ _ _)   = mkLast n
insnToG n@(Call _ _ _ _) = mkLast n
insnToG n@(Return _)     = mkLast n
