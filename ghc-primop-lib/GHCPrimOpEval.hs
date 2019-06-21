module GHCPrimOpEval where

import Data.Bits
import GHCPrimOp


evalPrimOp :: PrimOp -> [Value] -> Value

-- Char
evalPrimOp CharGtOp [CharV a, CharV b] = IntV $ if a > b  then 1 else 0
evalPrimOp CharGeOp [CharV a, CharV b] = IntV $ if a >= b then 1 else 0
evalPrimOp CharEqOp [CharV a, CharV b] = IntV $ if a == b then 1 else 0
evalPrimOp CharNeOp [CharV a, CharV b] = IntV $ if a /= b then 1 else 0
evalPrimOp CharLtOp [CharV a, CharV b] = IntV $ if a < b  then 1 else 0
evalPrimOp CharLeOp [CharV a, CharV b] = IntV $ if a <= b then 1 else 0
evalPrimOp OrdOp    [CharV a] = IntV $ fromIntegral a -- HINT: noop ; same bit level representation

-- Int
evalPrimOp IntAddOp [IntV a, IntV b] = IntV $ a + b
evalPrimOp IntSubOp [IntV a, IntV b] = IntV $ a - b
evalPrimOp IntMulOp [IntV a, IntV b] = IntV $ a * b
{-
  TODO
    IntMulMayOfloOp
    IntQuotOp
    IntRemOp
    IntQuotRemOp
-}
evalPrimOp AndIOp   [IntV a, IntV b]  = IntV $ a .&. b
evalPrimOp OrIOp    [IntV a, IntV b]  = IntV $ a .|. b
evalPrimOp XorIOp   [IntV a, IntV b]  = IntV $ a `xor` b
evalPrimOp NotIOp   [IntV a]  = IntV $ complement a
evalPrimOp IntNegOp [IntV a]  = IntV (-a)
{-
  TODO
    IntAddCOp
    IntSubCOp
-}
evalPrimOp IntGtOp    [IntV a, IntV b] = IntV $ if a > b  then 1 else 0
evalPrimOp IntGeOp    [IntV a, IntV b] = IntV $ if a >= b then 1 else 0
evalPrimOp IntEqOp    [IntV a, IntV b] = IntV $ if a == b then 1 else 0
evalPrimOp IntNeOp    [IntV a, IntV b] = IntV $ if a /= b then 1 else 0
evalPrimOp IntLtOp    [IntV a, IntV b] = IntV $ if a < b  then 1 else 0
evalPrimOp IntLeOp    [IntV a, IntV b] = IntV $ if a <= b then 1 else 0
evalPrimOp ChrOp      [IntV a] = CharV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp Int2WordOp [IntV a] = WordV $ fromIntegral a -- HINT: noop ; same bit level representation
{-
   | Int2FloatOp
   | Int2DoubleOp
   | Word2FloatOp
   | Word2DoubleOp
-}
evalPrimOp ISllOp [IntV a, IntV b] = IntV $ unsafeShiftL a (fromIntegral b)
evalPrimOp ISraOp [IntV a, IntV b] = IntV $ unsafeShiftR a (fromIntegral b)

evalPrimOp _ _ = undefined

