module GHCPrimOpEval where

import Data.Int
import Data.Word
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
evalPrimOp IntAddOp         [IntV a, IntV b] = IntV $ a + b
evalPrimOp IntSubOp         [IntV a, IntV b] = IntV $ a - b
evalPrimOp IntMulOp         [IntV a, IntV b] = IntV $ a * b
evalPrimOp IntMulMayOfloOp  [IntV a, IntV b] = IntV $ if fromIntegral a * (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0
evalPrimOp IntQuotOp        [IntV a, IntV b] = IntV $ a `quot` b  -- NOTE: int / int in C
evalPrimOp IntRemOp         [IntV a, IntV b] = IntV $ a `rem` b   -- NOTE: int % int in C
evalPrimOp IntQuotRemOp     [IntV a, IntV b] = TupleV [IntV $ a `quot` b, IntV $ a `rem` b]
evalPrimOp AndIOp           [IntV a, IntV b] = IntV $ a .&. b
evalPrimOp OrIOp            [IntV a, IntV b] = IntV $ a .|. b
evalPrimOp XorIOp           [IntV a, IntV b] = IntV $ a `xor` b
evalPrimOp NotIOp           [IntV a] = IntV $ complement a
evalPrimOp IntNegOp         [IntV a] = IntV (-a)
evalPrimOp IntAddCOp [IntV a, IntV b] = TupleV
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0
                                        ]
evalPrimOp IntSubCOp [IntV a, IntV b] = TupleV
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimInt) then 1 else 0
                                        ]
evalPrimOp IntGtOp  [IntV a, IntV b] = IntV $ if a > b  then 1 else 0
evalPrimOp IntGeOp  [IntV a, IntV b] = IntV $ if a >= b then 1 else 0
evalPrimOp IntEqOp  [IntV a, IntV b] = IntV $ if a == b then 1 else 0
evalPrimOp IntNeOp  [IntV a, IntV b] = IntV $ if a /= b then 1 else 0
evalPrimOp IntLtOp  [IntV a, IntV b] = IntV $ if a < b  then 1 else 0
evalPrimOp IntLeOp  [IntV a, IntV b] = IntV $ if a <= b then 1 else 0
evalPrimOp ChrOp          [IntV a]  = CharV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp Int2WordOp     [IntV a]  = WordV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp Int2FloatOp    [IntV a]  = FloatV $ fromIntegral a
evalPrimOp Int2DoubleOp   [IntV a]  = DoubleV $ fromIntegral a
evalPrimOp Word2FloatOp   [WordV a] = FloatV $ fromIntegral a
evalPrimOp Word2DoubleOp  [WordV a] = DoubleV $ fromIntegral a
evalPrimOp ISllOp [IntV a, IntV b] = IntV $ unsafeShiftL a (fromIntegral b)
evalPrimOp ISraOp [IntV a, IntV b] = IntV $ unsafeShiftR a (fromIntegral b) -- Shift right arithmetic
evalPrimOp ISrlOp [IntV a, IntV b] = IntV $ fromIntegral $ unsafeShiftR (fromIntegral a :: PrimWord) (fromIntegral b) -- Shift right logical

-- Word
evalPrimOp WordAddOp  [WordV a, WordV b] = WordV $ a + b
evalPrimOp WordAddCOp [WordV a, WordV b] = TupleV
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimWord) then 1 else 0
                                        ]
evalPrimOp WordSubCOp [WordV a, WordV b] = TupleV
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimWord) then 1 else 0
                                        ]
evalPrimOp WordAdd2Op [WordV a, WordV b] = TupleV [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
  res = fromIntegral a + fromIntegral b :: Integer
  hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
  lo  = res - hi

evalPrimOp WordSubOp  [WordV a, WordV b] = WordV $ a - b
evalPrimOp WordMulOp  [WordV a, WordV b] = WordV $ a * b
evalPrimOp WordMul2Op [WordV a, WordV b] = TupleV [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
  res = fromIntegral a * fromIntegral b :: Integer
  hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
  lo  = res - hi

evalPrimOp WordQuotOp     [WordV a, WordV b] = WordV $ a `quot` b  -- NOTE: uint / uint in C
evalPrimOp WordRemOp      [WordV a, WordV b] = WordV $ a `rem` b   -- NOTE: uint % uint in C
evalPrimOp WordQuotRemOp  [WordV a, WordV b] = TupleV [WordV $ a `quot` b, WordV $ a `rem` b]
evalPrimOp WordQuotRem2Op [WordV hi, WordV lo, WordV b'] = TupleV [WordV . fromIntegral $ a `quot` b, WordV . fromIntegral $ a `rem` b] where
  a = fromIntegral hi * fromIntegral (1 + maxBound :: PrimWord) + fromIntegral lo :: Integer
  b = fromIntegral b' :: Integer

evalPrimOp AndOp  [WordV a, WordV b] = WordV $ a .&. b
evalPrimOp OrOp   [WordV a, WordV b] = WordV $ a .|. b
evalPrimOp XorOp  [WordV a, WordV b] = WordV $ a `xor` b
evalPrimOp NotOp  [WordV a] = WordV $ complement a
evalPrimOp SllOp  [WordV a, IntV b] = WordV $ unsafeShiftL a (fromIntegral b)
evalPrimOp SrlOp  [WordV a, IntV b] = WordV $ unsafeShiftR a (fromIntegral b) -- Shift right logical
evalPrimOp Word2IntOp [WordV a] = IntV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp WordGtOp   [WordV a, WordV b] = IntV $ if a > b  then 1 else 0
evalPrimOp WordGeOp   [WordV a, WordV b] = IntV $ if a >= b then 1 else 0
evalPrimOp WordEqOp   [WordV a, WordV b] = IntV $ if a == b then 1 else 0
evalPrimOp WordNeOp   [WordV a, WordV b] = IntV $ if a /= b then 1 else 0
evalPrimOp WordLtOp   [WordV a, WordV b] = IntV $ if a < b  then 1 else 0
evalPrimOp WordLeOp   [WordV a, WordV b] = IntV $ if a <= b then 1 else 0

evalPrimOp op args = error $ "unsupported op: " ++ show op ++ " with args: " ++ show args


