{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Lambda.GHCPrimOps where

import qualified Data.Set as Set
import Lambda.Syntax
import Lambda.TH

primPrelude :: Program
primPrelude = [progConst|
  {-
    Char#
  -}
  primop pure
    "gtChar#" :: T_Char -> T_Char -> T_Int64
    "geChar#" :: T_Char -> T_Char -> T_Int64
    "eqChar#" :: T_Char -> T_Char -> T_Int64
    "neChar#" :: T_Char -> T_Char -> T_Int64
    "ltChar#" :: T_Char -> T_Char -> T_Int64
    "leChar#" :: T_Char -> T_Char -> T_Int64
    "ord#"    :: T_Char -> T_Int64

  {-
    Int#
  -}
  primop pure
    "+#"                 :: T_Int64 -> T_Int64 -> T_Int64
    "-#"                 :: T_Int64 -> T_Int64 -> T_Int64
    "*#"                 :: T_Int64 -> T_Int64 -> T_Int64
    "mulIntMayOflo#"     :: T_Int64 -> T_Int64 -> T_Int64
    "quotInt#"           :: T_Int64 -> T_Int64 -> T_Int64
    "remInt#"            :: T_Int64 -> T_Int64 -> T_Int64
    "quotRemInt#"        :: T_Int64 -> T_Int64 -> {"GHC.Prim.(#,#)" T_Int64 T_Int64}
    "andI#"              :: T_Int64 -> T_Int64 -> T_Int64
    "orI#"               :: T_Int64 -> T_Int64 -> T_Int64
    "xorI#"              :: T_Int64 -> T_Int64 -> T_Int64
    "notI#"              :: T_Int64 -> T_Int64
    "negateInt#"         :: T_Int64 -> T_Int64
    "addIntC#"           :: T_Int64 -> T_Int64 -> {"GHC.Prim.(#,#)" T_Int64 T_Int64}
    "subIntC#"           :: T_Int64 -> T_Int64 -> {"GHC.Prim.(#,#)" T_Int64 T_Int64}
    ">#"                 :: T_Int64 -> T_Int64 -> T_Int64
    ">=#"                :: T_Int64 -> T_Int64 -> T_Int64
    "==#"                :: T_Int64 -> T_Int64 -> T_Int64
    "/=#"                :: T_Int64 -> T_Int64 -> T_Int64
    "<#"                 :: T_Int64 -> T_Int64 -> T_Int64
    "<=#"                :: T_Int64 -> T_Int64 -> T_Int64
    "chr#"               :: T_Int64 -> T_Char
    "int2Word#"          :: T_Int64 -> T_Word64
    "int2Float#"         :: T_Int64 -> T_Float
    "int2Double#"        :: T_Int64 -> T_Double
    "word2Float#"        :: T_Word64 -> T_Float
    "word2Double#"       :: T_Word64 -> T_Double
    "uncheckedIShiftL#"  :: T_Int64 -> T_Int64 -> T_Int64
    "uncheckedIShiftRA#" :: T_Int64 -> T_Int64 -> T_Int64
    "uncheckedIShiftRL#" :: T_Int64 -> T_Int64 -> T_Int64

  {-
    Word#
  -}
  primop pure
    "plusWord#"         :: T_Word64 -> T_Word64 -> T_Word64
    "addWordC#"         :: T_Word64 -> T_Word64 -> {"GHC.Prim.(#,#)" T_Word64 T_Int64}
    "subWordC#"         :: T_Word64 -> T_Word64 -> {"GHC.Prim.(#,#)" T_Word64 T_Int64}
    "plusWord2#"        :: T_Word64 -> T_Word64 -> {"GHC.Prim.(#,#)" T_Word64 T_Word64}
    "minusWord#"        :: T_Word64 -> T_Word64 -> T_Word64
    "timesWord#"        :: T_Word64 -> T_Word64 -> T_Word64
    "timesWord2#"       :: T_Word64 -> T_Word64 -> {"GHC.Prim.(#,#)" T_Word64 T_Word64}
    "quotWord#"         :: T_Word64 -> T_Word64 -> T_Word64
    "remWord#"          :: T_Word64 -> T_Word64 -> T_Word64
    "quotRemWord#"      :: T_Word64 -> T_Word64 -> {"GHC.Prim.(#,#)" T_Word64 T_Word64}
    "quotRemWord2#"     :: T_Word64 -> T_Word64 -> T_Word64 -> {"GHC.Prim.(#,#)" T_Word64 T_Word64}
    "and#"              :: T_Word64 -> T_Word64 -> T_Word64
    "or#"               :: T_Word64 -> T_Word64 -> T_Word64
    "xor#"              :: T_Word64 -> T_Word64 -> T_Word64
    "not#"              :: T_Word64 -> T_Word64
    "uncheckedShiftL#"  :: T_Word64 -> T_Int64 -> T_Word64
    "uncheckedShiftRL#" :: T_Word64 -> T_Int64 -> T_Word64
    "word2Int#"         :: T_Word64 -> T_Int64
    "gtWord#"           :: T_Word64 -> T_Word64 -> T_Int64
    "geWord#"           :: T_Word64 -> T_Word64 -> T_Int64
    "eqWord#"           :: T_Word64 -> T_Word64 -> T_Int64
    "neWord#"           :: T_Word64 -> T_Word64 -> T_Int64
    "ltWord#"           :: T_Word64 -> T_Word64 -> T_Int64
    "leWord#"           :: T_Word64 -> T_Word64 -> T_Int64
    "popCnt8#"          :: T_Word64 -> T_Word64
    "popCnt16#"         :: T_Word64 -> T_Word64
    "popCnt32#"         :: T_Word64 -> T_Word64
    "popCnt64#"         :: T_Word64 -> T_Word64
    "popCnt#"           :: T_Word64 -> T_Word64
    "pdep8#"            :: T_Word64 -> T_Word64 -> T_Word64
    "pdep16#"           :: T_Word64 -> T_Word64 -> T_Word64
    "pdep32#"           :: T_Word64 -> T_Word64 -> T_Word64
    "pdep64#"           :: T_Word64 -> T_Word64 -> T_Word64
    "pdep#"             :: T_Word64 -> T_Word64 -> T_Word64
    "pext8#"            :: T_Word64 -> T_Word64 -> T_Word64
    "pext16#"           :: T_Word64 -> T_Word64 -> T_Word64
    "pext32#"           :: T_Word64 -> T_Word64 -> T_Word64
    "pext64#"           :: T_Word64 -> T_Word64 -> T_Word64
    "pext#"             :: T_Word64 -> T_Word64 -> T_Word64
    "clz8#"             :: T_Word64 -> T_Word64
    "clz16#"            :: T_Word64 -> T_Word64
    "clz32#"            :: T_Word64 -> T_Word64
    "clz64#"            :: T_Word64 -> T_Word64
    "clz#"              :: T_Word64 -> T_Word64
    "ctz8#"             :: T_Word64 -> T_Word64
    "ctz16#"            :: T_Word64 -> T_Word64
    "ctz32#"            :: T_Word64 -> T_Word64
    "ctz64#"            :: T_Word64 -> T_Word64
    "ctz#"              :: T_Word64 -> T_Word64
    "byteSwap16#"       :: T_Word64 -> T_Word64
    "byteSwap32#"       :: T_Word64 -> T_Word64
    "byteSwap64#"       :: T_Word64 -> T_Word64
    "byteSwap#"         :: T_Word64 -> T_Word64

  {-
    Narrowings
  -}
  primop pure
    "narrow8Int#"   :: T_Int64 -> T_Int64
    "narrow16Int#"  :: T_Int64 -> T_Int64
    "narrow32Int#"  :: T_Int64 -> T_Int64
    "narrow8Word#"  :: T_Word64 -> T_Word64
    "narrow16Word#" :: T_Word64 -> T_Word64
    "narrow32Word#" :: T_Word64 -> T_Word64

  {-
    Double#
  -}
  primop pure
    ">##"                 :: T_Double -> T_Double -> T_Int64
    ">=##"                :: T_Double -> T_Double -> T_Int64
    "==##"                :: T_Double -> T_Double -> T_Int64
    "/=##"                :: T_Double -> T_Double -> T_Int64
    "<##"                 :: T_Double -> T_Double -> T_Int64
    "<=##"                :: T_Double -> T_Double -> T_Int64
    "+##"                 :: T_Double -> T_Double -> T_Double
    "-##"                 :: T_Double -> T_Double -> T_Double
    "*##"                 :: T_Double -> T_Double -> T_Double
    "/##"                 :: T_Double -> T_Double -> T_Double
    "negateDouble#"       :: T_Double -> T_Double
    "fabsDouble#"         :: T_Double -> T_Double
    "double2Int#"         :: T_Double -> T_Int64
    "double2Float#"       :: T_Double -> T_Float
    "expDouble#"          :: T_Double -> T_Double
    "logDouble#"          :: T_Double -> T_Double
    "sqrtDouble#"         :: T_Double -> T_Double
    "sinDouble#"          :: T_Double -> T_Double
    "cosDouble#"          :: T_Double -> T_Double
    "tanDouble#"          :: T_Double -> T_Double
    "asinDouble#"         :: T_Double -> T_Double
    "acosDouble#"         :: T_Double -> T_Double
    "atanDouble#"         :: T_Double -> T_Double
    "sinhDouble#"         :: T_Double -> T_Double
    "coshDouble#"         :: T_Double -> T_Double
    "tanhDouble#"         :: T_Double -> T_Double
    "**##"                :: T_Double -> T_Double -> T_Double
    "decodeDouble_2Int#"  :: T_Double -> {"GHC.Prim.(#,,,#)" T_Int64 T_Word64 T_Word64 T_Int64}
    "decodeDouble_Int64#" :: T_Double -> {"GHC.Prim.(#,#)" T_Int64 T_Int64}

  {-
    Float#
  -}
  primop pure
    "gtFloat#"         :: T_Float -> T_Float -> T_Int64
    "geFloat#"         :: T_Float -> T_Float -> T_Int64
    "eqFloat#"         :: T_Float -> T_Float -> T_Int64
    "neFloat#"         :: T_Float -> T_Float -> T_Int64
    "ltFloat#"         :: T_Float -> T_Float -> T_Int64
    "leFloat#"         :: T_Float -> T_Float -> T_Int64
    "plusFloat#"       :: T_Float -> T_Float -> T_Float
    "minusFloat#"      :: T_Float -> T_Float -> T_Float
    "timesFloat#"      :: T_Float -> T_Float -> T_Float
    "divideFloat#"     :: T_Float -> T_Float -> T_Float
    "negateFloat#"     :: T_Float -> T_Float
    "fabsFloat#"       :: T_Float -> T_Float
    "float2Int#"       :: T_Float -> T_Int64
    "expFloat#"        :: T_Float -> T_Float
    "logFloat#"        :: T_Float -> T_Float
    "sqrtFloat#"       :: T_Float -> T_Float
    "sinFloat#"        :: T_Float -> T_Float
    "cosFloat#"        :: T_Float -> T_Float
    "tanFloat#"        :: T_Float -> T_Float
    "asinFloat#"       :: T_Float -> T_Float
    "acosFloat#"       :: T_Float -> T_Float
    "atanFloat#"       :: T_Float -> T_Float
    "sinhFloat#"       :: T_Float -> T_Float
    "coshFloat#"       :: T_Float -> T_Float
    "tanhFloat#"       :: T_Float -> T_Float
    "powerFloat#"      :: T_Float -> T_Float -> T_Float
    "float2Double#"    :: T_Float -> T_Double
    "decodeFloat_Int#" :: T_Float -> {"GHC.Prim.(#,#)" T_Int64 T_Int64}

  {-
    Arrays
  -}
  primop effectful
    "newArray#" :: T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}

  primop pure
    "sameMutableArray#" :: {"MutableArray#" %s %a} -> {"MutableArray#" %s %a} -> T_Int64

  primop effectful
    "readArray#"  :: {"MutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    "writeArray#" :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    "sizeofArray#"        :: {"Array#" %a} -> T_Int64
    "sizeofMutableArray#" :: {"MutableArray#" %s %a} -> T_Int64
    "indexArray#"         :: {"Array#" %a} -> T_Int64 -> {"GHC.Prim.Unit#" %a}

  primop effectful
    "unsafeFreezeArray#" :: {"MutableArray#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"Array#" %a}}
    "unsafeThawArray#"   :: {"Array#" %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    "copyArray#"         :: {"Array#" %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copyMutableArray#"  :: {"MutableArray#" %s %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "cloneArray#"        :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"Array#" %a}
    "cloneMutableArray#" :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    "freezeArray#"       :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"Array#" %a}}
    "thawArray#"         :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArray#" %s %a}}
    "casArray#"          :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}

  {-
    Small Arrays
  -}
  primop effectful
    "newSmallArray#" :: T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}

  primop pure
    "sameSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> {"SmallMutableArray#" %s %a} -> T_Int64

  primop effectful
    "readSmallArray#"  :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    "writeSmallArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    "sizeofSmallArray#"        :: {"SmallArray#" %a} -> T_Int64
    "sizeofSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64
    "indexSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> {"GHC.Prim.Unit#" %a}

  primop effectful
    "unsafeFreezeSmallArray#" :: {"SmallMutableArray#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallArray#" %a}}
    "unsafeThawSmallArray#"   :: {"SmallArray#" %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    "copySmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copySmallMutableArray#"  :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "cloneSmallArray#"        :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"SmallArray#" %a}
    "cloneSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    "freezeSmallArray#"       :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallArray#" %a}}
    "thawSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s %a}}
    "casSmallArray#"          :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}

  {-
    Byte Arrays
  -}
  primop effectful
    "newByteArray#"              :: T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s}}
    "newPinnedByteArray#"        :: T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s}}
    "newAlignedPinnedByteArray#" :: T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s}}

  primop pure
    "isMutableByteArrayPinned#" :: {"MutableByteArray#" %s} -> T_Int64
    "isByteArrayPinned#"        :: {"ByteArray#"} -> T_Int64
    "byteArrayContents#"        :: {"ByteArray#"} -> T_Addr
    "sameMutableByteArray#"     :: {"MutableByteArray#" %s} -> {"MutableByteArray#" %s} -> T_Int64

  primop effectful
    "shrinkMutableByteArray#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "resizeMutableByteArray#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s}}
    "unsafeFreezeByteArray#"  :: {"MutableByteArray#" %s} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"ByteArray#"}}

  primop pure
    "sizeofByteArray#"            :: {"ByteArray#"} -> T_Int64
    "sizeofMutableByteArray#"     :: {"MutableByteArray#" %s} -> T_Int64
    "getSizeofMutableByteArray#"  :: {"MutableByteArray#" %s} -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "indexCharArray#"             :: {"ByteArray#"} -> T_Int64 -> T_Char
    "indexWideCharArray#"         :: {"ByteArray#"} -> T_Int64 -> T_Char
    "indexIntArray#"              :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWordArray#"             :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexAddrArray#"             :: {"ByteArray#"} -> T_Int64 -> T_Addr
    "indexFloatArray#"            :: {"ByteArray#"} -> T_Int64 -> T_Float
    "indexDoubleArray#"           :: {"ByteArray#"} -> T_Int64 -> T_Double
    "indexStablePtrArray#"        :: {"ByteArray#"} -> T_Int64 -> {"StablePtr#" %a}
    "indexInt8Array#"             :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexInt16Array#"            :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexInt32Array#"            :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexInt64Array#"            :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8Array#"            :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord16Array#"           :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord32Array#"           :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord64Array#"           :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord8ArrayAsChar#"      :: {"ByteArray#"} -> T_Int64 -> T_Char
    "indexWord8ArrayAsWideChar#"  :: {"ByteArray#"} -> T_Int64 -> T_Char
    "indexWord8ArrayAsAddr#"      :: {"ByteArray#"} -> T_Int64 -> T_Addr
    "indexWord8ArrayAsFloat#"     :: {"ByteArray#"} -> T_Int64 -> T_Float
    "indexWord8ArrayAsDouble#"    :: {"ByteArray#"} -> T_Int64 -> T_Double
    "indexWord8ArrayAsStablePtr#" :: {"ByteArray#"} -> T_Int64 -> {"StablePtr#" %a}
    "indexWord8ArrayAsInt16#"     :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsInt32#"     :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsInt64#"     :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsInt#"       :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsWord16#"    :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord8ArrayAsWord32#"    :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord8ArrayAsWord64#"    :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord8ArrayAsWord#"      :: {"ByteArray#"} -> T_Int64 -> T_Word64

  primop effectful
    "readCharArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Char}
    "readWideCharArray#"          :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Char}
    "readIntArray#"               :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWordArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readAddrArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Addr}
    "readFloatArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Float}
    "readDoubleArray#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Double}
    "readStablePtrArray#"         :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"StablePtr#" %a}}
    "readInt8Array#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readInt16Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readInt32Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readInt64Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWord8Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord16Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord32Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord64Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord8ArrayAsChar#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Char}
    "readWord8ArrayAsWideChar#"   :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Char}
    "readWord8ArrayAsAddr#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Addr}
    "readWord8ArrayAsFloat#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Float}
    "readWord8ArrayAsDouble#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Double}
    "readWord8ArrayAsStablePtr#"  :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"StablePtr#" %a}}
    "readWord8ArrayAsInt16#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWord8ArrayAsInt32#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWord8ArrayAsInt64#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWord8ArrayAsInt#"        :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWord8ArrayAsWord16#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord8ArrayAsWord32#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord8ArrayAsWord64#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord8ArrayAsWord#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "writeCharArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWideCharArray#"         :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeIntArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWordArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeAddrArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Addr -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeFloatArray#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Float -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeDoubleArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Double -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeStablePtrArray#"        :: {"MutableByteArray#" %s} -> T_Int64 -> {"StablePtr#" %a} -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt8Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt16Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt32Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt64Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord16Array#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord32Array#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord64Array#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsChar#"      :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsWideChar#"  :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsAddr#"      :: {"MutableByteArray#" %s} -> T_Int64 -> T_Addr -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsFloat#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Float -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsDouble#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Double -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsStablePtr#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"StablePtr#" %a} -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsInt16#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsInt32#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsInt64#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsInt#"       :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsWord16#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsWord32#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsWord64#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8ArrayAsWord#"      :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    "compareByteArrays#" :: {"ByteArray#"} -> T_Int64 -> {"ByteArray#"} -> T_Int64 -> T_Int64 -> T_Int64

  primop effectful
    "copyByteArray#"              :: {"ByteArray#"} -> T_Int64 -> {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copyMutableByteArray#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copyByteArrayToAddr#"        :: {"ByteArray#"} -> T_Int64 -> T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copyMutableByteArrayToAddr#" :: {"MutableByteArray#" %s} -> T_Int64 -> T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copyAddrToByteArray#"        :: T_Addr -> {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "setByteArray#"               :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "atomicReadIntArray#"         :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "atomicWriteIntArray#"        :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "casIntArray#"                :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "fetchAddIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "fetchSubIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "fetchAndIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "fetchNandIntArray#"          :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "fetchOrIntArray#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "fetchXorIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}

  {-
    Arrays of arrays
  -}
  primop effectful
    "newArrayArray#" :: T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArrayArray#" %s}}

  primop pure
    "sameMutableArrayArray#" :: {"MutableArrayArray#" %s} -> {"MutableArrayArray#" %s} -> T_Int64

  primop effectful
    "unsafeFreezeArrayArray#" :: {"MutableArrayArray#" %s} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"ArrayArray#"}}

  primop pure
    "sizeofArrayArray#"        :: {"ArrayArray#"} -> T_Int64
    "sizeofMutableArrayArray#" :: {"MutableArrayArray#" %s} -> T_Int64
    "indexByteArrayArray#"     :: {"ArrayArray#"} -> T_Int64 -> {"ByteArray#"}
    "indexArrayArrayArray#"    :: {"ArrayArray#"} -> T_Int64 -> {"ArrayArray#"}

  primop effectful
    "readByteArrayArray#"          :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"ByteArray#"}}
    "readMutableByteArrayArray#"   :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s}}
    "readArrayArrayArray#"         :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"ArrayArray#"}}
    "readMutableArrayArrayArray#"  :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutableArrayArray#" %s}}
    "writeByteArrayArray#"         :: {"MutableArrayArray#" %s} -> T_Int64 -> {"ByteArray#"} -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeMutableByteArrayArray#"  :: {"MutableArrayArray#" %s} -> T_Int64 -> {"MutableByteArray#" %s} -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeArrayArrayArray#"        :: {"MutableArrayArray#" %s} -> T_Int64 -> {"ArrayArray#"} -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeMutableArrayArrayArray#" :: {"MutableArrayArray#" %s} -> T_Int64 -> {"MutableArrayArray#" %s} -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copyArrayArray#"              :: {"ArrayArray#"} -> T_Int64 -> {"MutableArrayArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "copyMutableArrayArray#"       :: {"MutableArrayArray#" %s} -> T_Int64 -> {"MutableArrayArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}

  {-
    Addr#
  -}
  primop pure
    "plusAddr#"              :: T_Addr -> T_Int64 -> T_Addr
    "minusAddr#"             :: T_Addr -> T_Addr -> T_Int64
    "remAddr#"               :: T_Addr -> T_Int64 -> T_Int64
    "addr2Int#"              :: T_Addr -> T_Int64
    "int2Addr#"              :: T_Int64 -> T_Addr
    "gtAddr#"                :: T_Addr -> T_Addr -> T_Int64
    "geAddr#"                :: T_Addr -> T_Addr -> T_Int64
    "eqAddr#"                :: T_Addr -> T_Addr -> T_Int64
    "neAddr#"                :: T_Addr -> T_Addr -> T_Int64
    "ltAddr#"                :: T_Addr -> T_Addr -> T_Int64
    "leAddr#"                :: T_Addr -> T_Addr -> T_Int64
    "indexCharOffAddr#"      :: T_Addr -> T_Int64 -> T_Char
    "indexWideCharOffAddr#"  :: T_Addr -> T_Int64 -> T_Char
    "indexIntOffAddr#"       :: T_Addr -> T_Int64 -> T_Int64
    "indexWordOffAddr#"      :: T_Addr -> T_Int64 -> T_Word64
    "indexAddrOffAddr#"      :: T_Addr -> T_Int64 -> T_Addr
    "indexFloatOffAddr#"     :: T_Addr -> T_Int64 -> T_Float
    "indexDoubleOffAddr#"    :: T_Addr -> T_Int64 -> T_Double
    "indexStablePtrOffAddr#" :: T_Addr -> T_Int64 -> {"StablePtr#" %a}
    "indexInt8OffAddr#"      :: T_Addr -> T_Int64 -> T_Int64
    "indexInt16OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64
    "indexInt32OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64
    "indexInt64OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64
    "indexWord8OffAddr#"     :: T_Addr -> T_Int64 -> T_Word64
    "indexWord16OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64
    "indexWord32OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64
    "indexWord64OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64

  primop effectful
    "readCharOffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Char}
    "readWideCharOffAddr#"   :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Char}
    "readIntOffAddr#"        :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWordOffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readAddrOffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Addr}
    "readFloatOffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Float}
    "readDoubleOffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Double}
    "readStablePtrOffAddr#"  :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" {"StablePtr#" %a}}
    "readInt8OffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readInt16OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readInt32OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readInt64OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readWord8OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord16OffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord32OffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "readWord64OffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Word64}
    "writeCharOffAddr#"      :: T_Addr -> T_Int64 -> T_Char -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWideCharOffAddr#"  :: T_Addr -> T_Int64 -> T_Char -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeIntOffAddr#"       :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWordOffAddr#"      :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeAddrOffAddr#"      :: T_Addr -> T_Int64 -> T_Addr -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeFloatOffAddr#"     :: T_Addr -> T_Int64 -> T_Float -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeDoubleOffAddr#"    :: T_Addr -> T_Int64 -> T_Double -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeStablePtrOffAddr#" :: T_Addr -> T_Int64 -> {"StablePtr#" %a} -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt8OffAddr#"      :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt16OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt32OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeInt64OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord8OffAddr#"     :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord16OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord32OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "writeWord64OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"GHC.Prim.(##)"}

  {-
    Mutable variables
  -}
  primop effectful
    "newMutVar#"   :: %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"MutVar#" %s %a}}
    "readMutVar#"  :: {"MutVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    "writeMutVar#" :: {"MutVar#" %s %a} -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    "sameMutVar#" :: {"MutVar#" %s %a} -> {"MutVar#" %s %a} -> T_Int64

  primop effectful
    "atomicModifyMutVar#" :: {"MutVar#" %s %a} -> (%a -> %b) -> {"State#" %s} -> {"GHC.Prim.Unit#" %c}
    "casMutVar#"          :: {"MutVar#" %s %a} -> %a -> %a -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}

  {-
    Exceptions
  -}
  primop effectful
    "catch#"                 :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> (%b -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "raise#"                 :: %b -> %o
    "raiseIO#"               :: %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %b}
    "maskAsyncExceptions#"   :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "maskUninterruptible#"   :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "unmaskAsyncExceptions#" :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "getMaskingState#"       :: {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Int64}

  {-
    STM-accessible Mutable Variables
  -}
  primop effectful
    "atomically#" :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "retry#"      :: {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "catchRetry#" :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "catchSTM#"   :: ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> (%b -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "newTVar#"    :: %a -> {"State#" %s} -> {"GHC.Prim.Unit#" {"TVar#" %s %a}}
    "readTVar#"   :: {"TVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    "readTVarIO#" :: {"TVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    "writeTVar#"  :: {"TVar#" %s %a} -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  primop pure
    "sameTVar#" :: {"TVar#" %s %a} -> {"TVar#" %s %a} -> T_Int64

  {-
    Synchronized Mutable Variables
  -}
  primop effectful
    "newMVar#"     :: {"State#" %s} -> {"GHC.Prim.Unit#" {"MVar#" %s %a}}
    "takeMVar#"    :: {"MVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    "tryTakeMVar#" :: {"MVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}
    "putMVar#"     :: {"MVar#" %s %a} -> %a -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "tryPutMVar#"  :: {"MVar#" %s %a} -> %a -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}
    "readMVar#"    :: {"MVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}
    "tryReadMVar#" :: {"MVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}

  primop pure
    "sameMVar#" :: {"MVar#" %s %a} -> {"MVar#" %s %a} -> T_Int64

  primop effectful
    "isEmptyMVar#" :: {"MVar#" %s %a} -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}

  {-
    Delay/wait operations
  -}
  primop effectful
    "delay#"     :: T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "waitRead#"  :: T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "waitWrite#" :: T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}

  {-
    Concurrency primitives
  -}
  primop effectful
    "fork#"                 :: %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"ThreadId#"}}
    "forkOn#"               :: T_Int64 -> %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"ThreadId#"}}
    "killThread#"           :: {"ThreadId#"} -> %a -> {"State#" {RealWorld}} -> {"GHC.Prim.(##)"}
    "yield#"                :: {"State#" {RealWorld}} -> {"GHC.Prim.(##)"}
    "myThreadId#"           :: {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"ThreadId#"}}
    "labelThread#"          :: {"ThreadId#"} -> T_Addr -> {"State#" {RealWorld}} -> {"GHC.Prim.(##)"}
    "isCurrentThreadBound#" :: {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Int64}
    "noDuplicate#"          :: {"State#" %s} -> {"GHC.Prim.(##)"}
    "threadStatus#"         :: {"ThreadId#"} -> {"State#" {RealWorld}} -> {"GHC.Prim.(#,,#)" T_Int64 T_Int64 T_Int64}

  {-
    Weak pointers
  -}
  primop effectful
    "mkWeak#"              :: %o -> %b -> ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %c}) -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"Weak#" %b}}
    "mkWeakNoFinalizer#"   :: %o -> %b -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"Weak#" %b}}
    "addCFinalizerToWeak#" :: T_Addr -> T_Addr -> T_Int64 -> T_Addr -> {"Weak#" %b} -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Int64}
    "deRefWeak#"           :: {"Weak#" %a} -> {"State#" {RealWorld}} -> {"GHC.Prim.(#,#)" T_Int64 %a}
    "finalizeWeak#"        :: {"Weak#" %a} -> {"State#" {RealWorld}} -> {"GHC.Prim.(#,#)" T_Int64 ({"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %b})}
    "touch#"               :: %o -> {"State#" {RealWorld}} -> {"GHC.Prim.(##)"}

  {-
    Stable pointers and names
  -}
  primop effectful
    "makeStablePtr#"  :: %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"StablePtr#" %a}}
    "deRefStablePtr#" :: {"StablePtr#" %a} -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "eqStablePtr#"    :: {"StablePtr#" %a} -> {"StablePtr#" %a} -> T_Int64
    "makeStableName#" :: %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"StableName#" %a}}

  primop pure
    "eqStableName#"    :: {"StableName#" %a} -> {"StableName#" %b} -> T_Int64
    "stableNameToInt#" :: {"StableName#" %a} -> T_Int64

  {-
    Compact normal form
  -}
  primop effectful
    "compactNew#"    :: T_Word64 -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" {"Compact#"}}
    "compactResize#" :: {"Compact#"} -> T_Word64 -> {"State#" {RealWorld}} -> {"GHC.Prim.(##)"}

  primop pure
    "compactContains#"      :: {"Compact#"} -> %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Int64}
    "compactContainsAny#"   :: %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Int64}
    "compactGetFirstBlock#" :: {"Compact#"} -> {"State#" {RealWorld}} -> {"GHC.Prim.(#,#)" T_Addr T_Word64}
    "compactGetNextBlock#"  :: {"Compact#"} -> T_Addr -> {"State#" {RealWorld}} -> {"GHC.Prim.(#,#)" T_Addr T_Word64}

  primop effectful
    "compactAllocateBlock#"  :: T_Word64 -> T_Addr -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Addr}
    "compactFixupPointers#"  :: T_Addr -> T_Addr -> {"State#" {RealWorld}} -> {"GHC.Prim.(#,#)" {"Compact#"} T_Addr}
    "compactAdd#"            :: {"Compact#"} -> %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "compactAddWithSharing#" :: {"Compact#"} -> %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" %a}
    "compactSize#"           :: {"Compact#"} -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Word64}

  {-
    Unsafe pointer equality
  -}
  primop pure
    "reallyUnsafePtrEquality#" :: %a -> %a -> T_Int64

  {-
    Parallelism
  -}
  primop effectful
    "par#"   :: %a -> T_Int64
    "spark#" :: %a -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}

  primop pure
    "seq#" :: %a -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}

  primop effectful
    "getSpark#"  :: {"State#" %s} -> {"GHC.Prim.(#,#)" T_Int64 %a}
    "numSparks#" :: {"State#" %s} -> {"GHC.Prim.Unit#" T_Int64}

  {-
    Tag to enum stuff
  -}
  primop pure
    "dataToTag#" :: %a -> T_Int64
    "tagToEnum#" :: T_Int64 -> %a

  {-
    Bytecode operations
  -}
  primop pure
    "addrToAny#" :: T_Addr -> {"GHC.Prim.Unit#" %a}
    "anyToAddr#" :: %a -> {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Addr}
    "mkApUpd0#"  :: {"BCO#"} -> {"GHC.Prim.Unit#" %a}

  primop effectful
    "newBCO#" :: {"ByteArray#"} -> {"ByteArray#"} -> {"Array#" %a} -> T_Int64 -> {"ByteArray#"} -> {"State#" %s} -> {"GHC.Prim.Unit#" {"BCO#"}}

  primop pure
    "unpackClosure#" :: %a -> {"GHC.Prim.(#,,#)" T_Addr {"ByteArray#"} {"Array#" %b}}
    "getApStackVal#" :: %a -> T_Int64 -> {"GHC.Prim.(#,#)" T_Int64 %b}

  {-
    Misc
  -}
  primop pure
    "getCCSOf#"      :: %a -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Addr}
    "getCurrentCCS#" :: %a -> {"State#" %s} -> {"GHC.Prim.Unit#" T_Addr}
    "clearCCS#"      :: ({"State#" %s} -> {"GHC.Prim.Unit#" %a}) -> {"State#" %s} -> {"GHC.Prim.Unit#" %a}

  {-
    Etc
  -}
  primop effectful
    "traceEvent#"                 :: T_Addr -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "traceMarker#"                :: T_Addr -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "getThreadAllocationCounter#" :: {"State#" {RealWorld}} -> {"GHC.Prim.Unit#" T_Int64}
    "setThreadAllocationCounter#" :: T_Int64 -> {"State#" {RealWorld}} -> {"GHC.Prim.(##)"}

  {-
    Safe coercions
  -}

  {-
    SIMD Vectors
  -}

  {-
    Prefetch
  -}
  primop effectful
    "prefetchByteArray3#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchMutableByteArray3#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchAddr3#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchValue3#"            :: %a -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchByteArray2#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchMutableByteArray2#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchAddr2#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchValue2#"            :: %a -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchByteArray1#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchMutableByteArray1#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchAddr1#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchValue1#"            :: %a -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchByteArray0#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchMutableByteArray0#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchAddr0#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"GHC.Prim.(##)"}
    "prefetchValue0#"            :: %a -> {"State#" %s} -> {"GHC.Prim.(##)"}

  |]

unsupported :: Set.Set String
unsupported = Set.fromList

  -- Addr#
  [ "nullAddr#"                              -- pseudo ops are not supported

  -- Etc
  , "proxy#"                                 -- pseudo ops are not supported
  , "seq"                                    -- pseudo ops are not supported
  , "unsafeCoerce#"                          -- pseudo ops are not supported

  -- Safe coercions
  , "coerce"                                 -- pseudo ops are not supported

  -- SIMD Vectors
  , "broadcast#"                             -- unsupported type: TyF (TyApp SCALAR []) (TyApp VECTOR [])
  , "pack#"                                  -- unsupported type: TyF (TyApp VECTUPLE []) (TyApp VECTOR [])
  , "unpack#"                                -- unsupported type: TyF (TyApp VECTOR []) (TyApp VECTUPLE [])
  , "insert#"                                -- unsupported type: TyF (TyApp VECTOR []) (TyF (TyApp SCALAR []) (TyF (TyApp Int# []) (TyApp VECTOR [])))
  , "plus#"                                  -- unsupported type: TyF (TyApp VECTOR []) (TyF (TyApp VECTOR []) (TyApp VECTOR []))
  , "minus#"                                 -- unsupported type: TyF (TyApp VECTOR []) (TyF (TyApp VECTOR []) (TyApp VECTOR []))
  , "times#"                                 -- unsupported type: TyF (TyApp VECTOR []) (TyF (TyApp VECTOR []) (TyApp VECTOR []))
  , "divide#"                                -- unsupported type: TyF (TyApp VECTOR []) (TyF (TyApp VECTOR []) (TyApp VECTOR []))
  , "quot#"                                  -- unsupported type: TyF (TyApp VECTOR []) (TyF (TyApp VECTOR []) (TyApp VECTOR []))
  , "rem#"                                   -- unsupported type: TyF (TyApp VECTOR []) (TyF (TyApp VECTOR []) (TyApp VECTOR []))
  , "negate#"                                -- unsupported type: TyF (TyApp VECTOR []) (TyApp VECTOR [])
  , "indexArray#"                            -- unsupported type: TyF (TyApp ByteArray# []) (TyF (TyApp Int# []) (TyApp VECTOR []))
  , "readArray#"                             -- unsupported type: TyF (TyApp MutableByteArray# [TyVar "s"]) (TyF (TyApp Int# []) (TyF (TyApp State# [TyVar "s"]) (TyUTup [TyApp VECTOR []])))
  , "writeArray#"                            -- unsupported type: TyF (TyApp MutableByteArray# [TyVar "s"]) (TyF (TyApp Int# []) (TyF (TyApp VECTOR []) (TyF (TyApp State# [TyVar "s"]) (TyUTup []))))
  , "indexOffAddr#"                          -- unsupported type: TyF (TyApp Addr# []) (TyF (TyApp Int# []) (TyApp VECTOR []))
  , "readOffAddr#"                           -- unsupported type: TyF (TyApp Addr# []) (TyF (TyApp Int# []) (TyF (TyApp State# [TyVar "s"]) (TyUTup [TyApp VECTOR []])))
  , "writeOffAddr#"                          -- unsupported type: TyF (TyApp Addr# []) (TyF (TyApp Int# []) (TyF (TyApp VECTOR []) (TyF (TyApp State# [TyVar "s"]) (TyUTup []))))
  , "indexArrayAs#"                          -- unsupported type: TyF (TyApp ByteArray# []) (TyF (TyApp Int# []) (TyApp VECTOR []))
  , "readArrayAs#"                           -- unsupported type: TyF (TyApp MutableByteArray# [TyVar "s"]) (TyF (TyApp Int# []) (TyF (TyApp State# [TyVar "s"]) (TyUTup [TyApp VECTOR []])))
  , "writeArrayAs#"                          -- unsupported type: TyF (TyApp MutableByteArray# [TyVar "s"]) (TyF (TyApp Int# []) (TyF (TyApp VECTOR []) (TyF (TyApp State# [TyVar "s"]) (TyUTup []))))
  , "indexOffAddrAs#"                        -- unsupported type: TyF (TyApp Addr# []) (TyF (TyApp Int# []) (TyApp VECTOR []))
  , "readOffAddrAs#"                         -- unsupported type: TyF (TyApp Addr# []) (TyF (TyApp Int# []) (TyF (TyApp State# [TyVar "s"]) (TyUTup [TyApp VECTOR []])))
  , "writeOffAddrAs#"                        -- unsupported type: TyF (TyApp Addr# []) (TyF (TyApp Int# []) (TyF (TyApp VECTOR []) (TyF (TyApp State# [TyVar "s"]) (TyUTup []))))
  ]
