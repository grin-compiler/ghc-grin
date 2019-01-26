{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module GHCPrimOps where

import qualified Data.Set as Set
import Grin.Grin
import Grin.TH

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
    "quotRemInt#"        :: T_Int64 -> T_Int64 -> {Tup2 T_Int64 T_Int64}
    "andI#"              :: T_Int64 -> T_Int64 -> T_Int64
    "orI#"               :: T_Int64 -> T_Int64 -> T_Int64
    "xorI#"              :: T_Int64 -> T_Int64 -> T_Int64
    "notI#"              :: T_Int64 -> T_Int64
    "negateInt#"         :: T_Int64 -> T_Int64
    "addIntC#"           :: T_Int64 -> T_Int64 -> {Tup2 T_Int64 T_Int64}
    "subIntC#"           :: T_Int64 -> T_Int64 -> {Tup2 T_Int64 T_Int64}
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
    "addWordC#"         :: T_Word64 -> T_Word64 -> {Tup2 T_Word64 T_Int64}
    "subWordC#"         :: T_Word64 -> T_Word64 -> {Tup2 T_Word64 T_Int64}
    "plusWord2#"        :: T_Word64 -> T_Word64 -> {Tup2 T_Word64 T_Word64}
    "minusWord#"        :: T_Word64 -> T_Word64 -> T_Word64
    "timesWord#"        :: T_Word64 -> T_Word64 -> T_Word64
    "timesWord2#"       :: T_Word64 -> T_Word64 -> {Tup2 T_Word64 T_Word64}
    "quotWord#"         :: T_Word64 -> T_Word64 -> T_Word64
    "remWord#"          :: T_Word64 -> T_Word64 -> T_Word64
    "quotRemWord#"      :: T_Word64 -> T_Word64 -> {Tup2 T_Word64 T_Word64}
    "quotRemWord2#"     :: T_Word64 -> T_Word64 -> T_Word64 -> {Tup2 T_Word64 T_Word64}
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
    "decodeDouble_2Int#"  :: T_Double -> {Tup4 T_Int64 T_Word64 T_Word64 T_Int64}
    "decodeDouble_Int64#" :: T_Double -> {Tup2 T_Int64 T_Int64}

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
    "decodeFloat_Int#" :: T_Float -> {Tup2 T_Int64 T_Int64}

  {-
    Arrays
  -}
  primop effectful
    "newArray#"           :: T_Int64 -> %a -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableArray#" %s %a}}

  primop pure
    "sameMutableArray#"   :: {"MutableArray#" %s %a} -> {"MutableArray#" %s %a} -> T_Int64

  primop effectful
    "readArray#"          :: {"MutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} %a}
    "writeArray#"         :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"State#" %s}

  primop pure
    "sizeofArray#"        :: {"Array#" %a} -> T_Int64
    "sizeofMutableArray#" :: {"MutableArray#" %s %a} -> T_Int64
    "indexArray#"         :: {"Array#" %a} -> T_Int64 -> {Tup1 %a}

  primop effectful
    "unsafeFreezeArray#"  :: {"MutableArray#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} {"Array#" %a}}
    "unsafeThawArray#"    :: {"Array#" %a} -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableArray#" %s %a}}
    "copyArray#"          :: {"Array#" %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "copyMutableArray#"   :: {"MutableArray#" %s %a} -> T_Int64 -> {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "cloneArray#"         :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"Array#" %a}
    "cloneMutableArray#"  :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableArray#" %s %a}}
    "freezeArray#"        :: {"MutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"Array#" %a}}
    "thawArray#"          :: {"Array#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableArray#" %s %a}}
    "casArray#"           :: {"MutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {Tup3 {"State#" %s} T_Int64 %a}

  {-
    Small Arrays
  -}
  primop effectful
    "newSmallArray#"           :: T_Int64 -> %a -> {"State#" %s} -> {Tup2 {"State#" %s} {"SmallMutableArray#" %s %a}}

  primop pure
    "sameSmallMutableArray#"   :: {"SmallMutableArray#" %s %a} -> {"SmallMutableArray#" %s %a} -> T_Int64

  primop effectful
    "readSmallArray#"          :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} %a}
    "writeSmallArray#"         :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> {"State#" %s} -> {"State#" %s}

  primop pure
    "sizeofSmallArray#"        :: {"SmallArray#" %a} -> T_Int64
    "sizeofSmallMutableArray#" :: {"SmallMutableArray#" %s %a} -> T_Int64
    "indexSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> {Tup1 %a}

  primop effectful
    "unsafeFreezeSmallArray#"  :: {"SmallMutableArray#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} {"SmallArray#" %a}}
    "unsafeThawSmallArray#"    :: {"SmallArray#" %a} -> {"State#" %s} -> {Tup2 {"State#" %s} {"SmallMutableArray#" %s %a}}
    "copySmallArray#"          :: {"SmallArray#" %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "copySmallMutableArray#"   :: {"SmallMutableArray#" %s %a} -> T_Int64 -> {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "cloneSmallArray#"         :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"SmallArray#" %a}
    "cloneSmallMutableArray#"  :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"SmallMutableArray#" %s %a}}
    "freezeSmallArray#"        :: {"SmallMutableArray#" %s %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"SmallArray#" %a}}
    "thawSmallArray#"          :: {"SmallArray#" %a} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"SmallMutableArray#" %s %a}}
    "casSmallArray#"           :: {"SmallMutableArray#" %s %a} -> T_Int64 -> %a -> %a -> {"State#" %s} -> {Tup3 {"State#" %s} T_Int64 %a}

  {-
    Byte Arrays
  -}
  primop effectful
    "newByteArray#"               :: T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableByteArray#" %s}}
    "newPinnedByteArray#"         :: T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableByteArray#" %s}}
    "newAlignedPinnedByteArray#"  :: T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableByteArray#" %s}}

  primop pure
    "isMutableByteArrayPinned#"   :: {"MutableByteArray#" %s} -> T_Int64
    "isByteArrayPinned#"          :: {"ByteArray#"} -> T_Int64
    "byteArrayContents#"          :: {"ByteArray#"} -> T_Addr
    "sameMutableByteArray#"       :: {"MutableByteArray#" %s} -> {"MutableByteArray#" %s} -> T_Int64

  primop effectful
    "shrinkMutableByteArray#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "resizeMutableByteArray#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableByteArray#" %s}}
    "unsafeFreezeByteArray#"      :: {"MutableByteArray#" %s} -> {"State#" %s} -> {Tup2 {"State#" %s} {"ByteArray#"}}

  primop pure
    "sizeofByteArray#"            :: {"ByteArray#"} -> T_Int64
    "sizeofMutableByteArray#"     :: {"MutableByteArray#" %s} -> T_Int64
    "getSizeofMutableByteArray#"  :: {"MutableByteArray#" %s} -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "indexCharArray#"             :: {"ByteArray#"} -> T_Int64 -> T_Char
    "indexWideCharArray#"         :: {"ByteArray#"} -> T_Int64 -> T_Char
    "indexIntArray#"              :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWordArray#"             :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexAddrArray#"             :: {"ByteArray#"} -> T_Int64 -> T_Addr
    "indexFloatArray#"            :: {"ByteArray#"} -> T_Int64 -> T_Float
    "indexDoubleArray#"           :: {"ByteArray#"} -> T_Int64 -> T_Double
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
    "indexWord8ArrayAsInt16#"     :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsInt32#"     :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsInt64#"     :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsInt#"       :: {"ByteArray#"} -> T_Int64 -> T_Int64
    "indexWord8ArrayAsWord16#"    :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord8ArrayAsWord32#"    :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord8ArrayAsWord64#"    :: {"ByteArray#"} -> T_Int64 -> T_Word64
    "indexWord8ArrayAsWord#"      :: {"ByteArray#"} -> T_Int64 -> T_Word64

  primop effectful
    "readCharArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Char}
    "readWideCharArray#"          :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Char}
    "readIntArray#"               :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWordArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readAddrArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Addr}
    "readFloatArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Float}
    "readDoubleArray#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Double}
    "readInt8Array#"              :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readInt16Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readInt32Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readInt64Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWord8Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord16Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord32Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord64Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord8ArrayAsChar#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Char}
    "readWord8ArrayAsWideChar#"   :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Char}
    "readWord8ArrayAsAddr#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Addr}
    "readWord8ArrayAsFloat#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Float}
    "readWord8ArrayAsDouble#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Double}
    "readWord8ArrayAsInt16#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWord8ArrayAsInt32#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWord8ArrayAsInt64#"      :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWord8ArrayAsInt#"        :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWord8ArrayAsWord16#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord8ArrayAsWord32#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord8ArrayAsWord64#"     :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord8ArrayAsWord#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "writeCharArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"State#" %s}
    "writeWideCharArray#"         :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"State#" %s}
    "writeIntArray#"              :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWordArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeAddrArray#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Addr -> {"State#" %s} -> {"State#" %s}
    "writeFloatArray#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Float -> {"State#" %s} -> {"State#" %s}
    "writeDoubleArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Double -> {"State#" %s} -> {"State#" %s}
    "writeStablePtrArray#"        :: {"MutableByteArray#" %s} -> T_Int64 -> {"StablePtr#" %a} -> {"State#" %s} -> {"State#" %s}
    "writeInt8Array#"             :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeInt16Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeInt32Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeInt64Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8Array#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord16Array#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord32Array#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord64Array#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsChar#"      :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsWideChar#"  :: {"MutableByteArray#" %s} -> T_Int64 -> T_Char -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsAddr#"      :: {"MutableByteArray#" %s} -> T_Int64 -> T_Addr -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsFloat#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Float -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsDouble#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Double -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsStablePtr#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"StablePtr#" %a} -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsInt16#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsInt32#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsInt64#"     :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsInt#"       :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsWord16#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsWord32#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsWord64#"    :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8ArrayAsWord#"      :: {"MutableByteArray#" %s} -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}

  primop pure
    "compareByteArrays#"          :: {"ByteArray#"} -> T_Int64 -> {"ByteArray#"} -> T_Int64 -> T_Int64 -> T_Int64

  primop effectful
    "copyByteArray#"              :: {"ByteArray#"} -> T_Int64 -> {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "copyMutableByteArray#"       :: {"MutableByteArray#" %s} -> T_Int64 -> {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "copyByteArrayToAddr#"        :: {"ByteArray#"} -> T_Int64 -> T_Addr -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "copyMutableByteArrayToAddr#" :: {"MutableByteArray#" %s} -> T_Int64 -> T_Addr -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "copyAddrToByteArray#"        :: T_Addr -> {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "setByteArray#"               :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "atomicReadIntArray#"         :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "atomicWriteIntArray#"        :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "casIntArray#"                :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "fetchAddIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "fetchSubIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "fetchAndIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "fetchNandIntArray#"          :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "fetchOrIntArray#"            :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "fetchXorIntArray#"           :: {"MutableByteArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}

  {-
    Arrays of arrays
  -}
  primop effectful
    "newArrayArray#"               :: T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableArrayArray#" %s}}

  primop pure
    "sameMutableArrayArray#"       :: {"MutableArrayArray#" %s} -> {"MutableArrayArray#" %s} -> T_Int64

  primop effectful
    "unsafeFreezeArrayArray#"      :: {"MutableArrayArray#" %s} -> {"State#" %s} -> {Tup2 {"State#" %s} {"ArrayArray#"}}

  primop pure
    "sizeofArrayArray#"            :: {"ArrayArray#"} -> T_Int64
    "sizeofMutableArrayArray#"     :: {"MutableArrayArray#" %s} -> T_Int64
    "indexByteArrayArray#"         :: {"ArrayArray#"} -> T_Int64 -> {"ByteArray#"}
    "indexArrayArrayArray#"        :: {"ArrayArray#"} -> T_Int64 -> {"ArrayArray#"}

  primop effectful
    "readByteArrayArray#"          :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"ByteArray#"}}
    "readMutableByteArrayArray#"   :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableByteArray#" %s}}
    "readArrayArrayArray#"         :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"ArrayArray#"}}
    "readMutableArrayArrayArray#"  :: {"MutableArrayArray#" %s} -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutableArrayArray#" %s}}
    "writeByteArrayArray#"         :: {"MutableArrayArray#" %s} -> T_Int64 -> {"ByteArray#"} -> {"State#" %s} -> {"State#" %s}
    "writeMutableByteArrayArray#"  :: {"MutableArrayArray#" %s} -> T_Int64 -> {"MutableByteArray#" %s} -> {"State#" %s} -> {"State#" %s}
    "writeArrayArrayArray#"        :: {"MutableArrayArray#" %s} -> T_Int64 -> {"ArrayArray#"} -> {"State#" %s} -> {"State#" %s}
    "writeMutableArrayArrayArray#" :: {"MutableArrayArray#" %s} -> T_Int64 -> {"MutableArrayArray#" %s} -> {"State#" %s} -> {"State#" %s}
    "copyArrayArray#"              :: {"ArrayArray#"} -> T_Int64 -> {"MutableArrayArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "copyMutableArrayArray#"       :: {"MutableArrayArray#" %s} -> T_Int64 -> {"MutableArrayArray#" %s} -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}

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
    "indexInt8OffAddr#"      :: T_Addr -> T_Int64 -> T_Int64
    "indexInt16OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64
    "indexInt32OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64
    "indexInt64OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64
    "indexWord8OffAddr#"     :: T_Addr -> T_Int64 -> T_Word64
    "indexWord16OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64
    "indexWord32OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64
    "indexWord64OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64

  primop effectful
    "readCharOffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Char}
    "readWideCharOffAddr#"   :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Char}
    "readIntOffAddr#"        :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWordOffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readAddrOffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Addr}
    "readFloatOffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Float}
    "readDoubleOffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Double}
    "readInt8OffAddr#"       :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readInt16OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readInt32OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readInt64OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readWord8OffAddr#"      :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord16OffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord32OffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "readWord64OffAddr#"     :: T_Addr -> T_Int64 -> {"State#" %s} -> {Tup2 {"State#" %s} T_Word64}
    "writeCharOffAddr#"      :: T_Addr -> T_Int64 -> T_Char -> {"State#" %s} -> {"State#" %s}
    "writeWideCharOffAddr#"  :: T_Addr -> T_Int64 -> T_Char -> {"State#" %s} -> {"State#" %s}
    "writeIntOffAddr#"       :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWordOffAddr#"      :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeAddrOffAddr#"      :: T_Addr -> T_Int64 -> T_Addr -> {"State#" %s} -> {"State#" %s}
    "writeFloatOffAddr#"     :: T_Addr -> T_Int64 -> T_Float -> {"State#" %s} -> {"State#" %s}
    "writeDoubleOffAddr#"    :: T_Addr -> T_Int64 -> T_Double -> {"State#" %s} -> {"State#" %s}
    "writeStablePtrOffAddr#" :: T_Addr -> T_Int64 -> {"StablePtr#" %a} -> {"State#" %s} -> {"State#" %s}
    "writeInt8OffAddr#"      :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeInt16OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeInt32OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeInt64OffAddr#"     :: T_Addr -> T_Int64 -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "writeWord8OffAddr#"     :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord16OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord32OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}
    "writeWord64OffAddr#"    :: T_Addr -> T_Int64 -> T_Word64 -> {"State#" %s} -> {"State#" %s}

  {-
    Mutable variables
  -}
  primop effectful
    "newMutVar#"   :: %a -> {"State#" %s} -> {Tup2 {"State#" %s} {"MutVar#" %s %a}}
    "readMutVar#"  :: {"MutVar#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} %a}
    "writeMutVar#" :: {"MutVar#" %s %a} -> %a -> {"State#" %s} -> {"State#" %s}

  primop pure
    "sameMutVar#"  :: {"MutVar#" %s %a} -> {"MutVar#" %s %a} -> T_Int64

  primop effectful
    "casMutVar#"   :: {"MutVar#" %s %a} -> %a -> %a -> {"State#" %s} -> {Tup3 {"State#" %s} T_Int64 %a}

  {-
    Exceptions
  -}
  primop effectful
    "getMaskingState#" :: {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Int64}

  {-
    STM-accessible Mutable Variables
  -}
  primop effectful
    "newTVar#"    :: %a -> {"State#" %s} -> {Tup2 {"State#" %s} {"TVar#" %s %a}}
    "readTVar#"   :: {"TVar#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} %a}
    "readTVarIO#" :: {"TVar#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} %a}
    "writeTVar#"  :: {"TVar#" %s %a} -> %a -> {"State#" %s} -> {"State#" %s}

  primop pure
    "sameTVar#"   :: {"TVar#" %s %a} -> {"TVar#" %s %a} -> T_Int64

  {-
    Synchronized Mutable Variables
  -}
  primop effectful
    "takeMVar#"    :: {"MVar#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} %a}
    "tryTakeMVar#" :: {"MVar#" %s %a} -> {"State#" %s} -> {Tup3 {"State#" %s} T_Int64 %a}
    "putMVar#"     :: {"MVar#" %s %a} -> %a -> {"State#" %s} -> {"State#" %s}
    "tryPutMVar#"  :: {"MVar#" %s %a} -> %a -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}
    "readMVar#"    :: {"MVar#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} %a}
    "tryReadMVar#" :: {"MVar#" %s %a} -> {"State#" %s} -> {Tup3 {"State#" %s} T_Int64 %a}

  primop pure
    "sameMVar#"    :: {"MVar#" %s %a} -> {"MVar#" %s %a} -> T_Int64

  primop effectful
    "isEmptyMVar#" :: {"MVar#" %s %a} -> {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}

  {-
    Delay/wait operations
  -}
  primop effectful
    "delay#"     :: T_Int64 -> {"State#" %s} -> {"State#" %s}
    "waitRead#"  :: T_Int64 -> {"State#" %s} -> {"State#" %s}
    "waitWrite#" :: T_Int64 -> {"State#" %s} -> {"State#" %s}

  {-
    Concurrency primitives
  -}
  primop effectful
    "fork#"                 :: %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} {"ThreadId#"}}
    "forkOn#"               :: T_Int64 -> %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} {"ThreadId#"}}
    "killThread#"           :: {"ThreadId#"} -> %a -> {"State#" {RealWorld}} -> {"State#" {RealWorld}}
    "yield#"                :: {"State#" {RealWorld}} -> {"State#" {RealWorld}}
    "myThreadId#"           :: {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} {"ThreadId#"}}
    "labelThread#"          :: {"ThreadId#"} -> T_Addr -> {"State#" {RealWorld}} -> {"State#" {RealWorld}}
    "isCurrentThreadBound#" :: {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Int64}
    "noDuplicate#"          :: {"State#" %s} -> {"State#" %s}
    "threadStatus#"         :: {"ThreadId#"} -> {"State#" {RealWorld}} -> {Tup4 {"State#" {RealWorld}} T_Int64 T_Int64 T_Int64}

  {-
    Weak pointers
  -}
  primop effectful
    "mkWeakNoFinalizer#"   :: %o -> %b -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} {"Weak#" %b}}
    "addCFinalizerToWeak#" :: T_Addr -> T_Addr -> T_Int64 -> T_Addr -> {"Weak#" %b} -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Int64}
    "deRefWeak#"           :: {"Weak#" %a} -> {"State#" {RealWorld}} -> {Tup3 {"State#" {RealWorld}} T_Int64 %a}
    "touch#"               :: %o -> {"State#" {RealWorld}} -> {"State#" {RealWorld}}

  {-
    Stable pointers and names
  -}
  primop effectful
    "makeStablePtr#"   :: %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} {"StablePtr#" %a}}
    "deRefStablePtr#"  :: {"StablePtr#" %a} -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} %a}
    "eqStablePtr#"     :: {"StablePtr#" %a} -> {"StablePtr#" %a} -> T_Int64
    "makeStableName#"  :: %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} {"StableName#" %a}}

  primop pure
    "eqStableName#"    :: {"StableName#" %a} -> {"StableName#" %b} -> T_Int64
    "stableNameToInt#" :: {"StableName#" %a} -> T_Int64

  {-
    Compact normal form
  -}
  primop effectful
    "compactNew#"            :: T_Word64 -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} {"Compact#"}}
    "compactResize#"         :: {"Compact#"} -> T_Word64 -> {"State#" {RealWorld}} -> {"State#" {RealWorld}}

  primop pure
    "compactContains#"       :: {"Compact#"} -> %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Int64}
    "compactContainsAny#"    :: %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Int64}
    "compactGetFirstBlock#"  :: {"Compact#"} -> {"State#" {RealWorld}} -> {Tup3 {"State#" {RealWorld}} T_Addr T_Word64}
    "compactGetNextBlock#"   :: {"Compact#"} -> T_Addr -> {"State#" {RealWorld}} -> {Tup3 {"State#" {RealWorld}} T_Addr T_Word64}

  primop effectful
    "compactAllocateBlock#"  :: T_Word64 -> T_Addr -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Addr}
    "compactFixupPointers#"  :: T_Addr -> T_Addr -> {"State#" {RealWorld}} -> {Tup3 {"State#" {RealWorld}} {"Compact#"} T_Addr}
    "compactAdd#"            :: {"Compact#"} -> %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} %a}
    "compactAddWithSharing#" :: {"Compact#"} -> %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} %a}
    "compactSize#"           :: {"Compact#"} -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Word64}

  {-
    Unsafe pointer equality
  -}
  primop pure
    "reallyUnsafePtrEquality#" :: %a -> %a -> T_Int64

  {-
    Parallelism
  -}
  primop effectful
    "par#"       :: %a -> T_Int64
    "spark#"     :: %a -> {"State#" %s} -> {Tup2 {"State#" %s} %a}

  primop pure
    "seq#"       :: %a -> {"State#" %s} -> {Tup2 {"State#" %s} %a}

  primop effectful
    "numSparks#" :: {"State#" %s} -> {Tup2 {"State#" %s} T_Int64}

  {-
    Tag to enum stuff
  -}
  primop pure
    "dataToTag#" :: %a -> T_Int64

  {-
    Bytecode operations
  -}
  primop pure
    "anyToAddr#" :: %a -> {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Addr}

  primop effectful
    "newBCO#"    :: {"ByteArray#"} -> {"ByteArray#"} -> {"Array#" %a} -> T_Int64 -> {"ByteArray#"} -> {"State#" %s} -> {Tup2 {"State#" %s} {"BCO#"}}

  {-
    Misc
  -}
  primop pure
    "getCCSOf#"      :: %a -> {"State#" %s} -> {Tup2 {"State#" %s} T_Addr}
    "getCurrentCCS#" :: %a -> {"State#" %s} -> {Tup2 {"State#" %s} T_Addr}

  {-
    Etc
  -}
  primop effectful
    "traceEvent#"                 :: T_Addr -> {"State#" %s} -> {"State#" %s}
    "traceMarker#"                :: T_Addr -> {"State#" %s} -> {"State#" %s}
    "getThreadAllocationCounter#" :: {"State#" {RealWorld}} -> {Tup2 {"State#" {RealWorld}} T_Int64}
    "setThreadAllocationCounter#" :: T_Int64 -> {"State#" {RealWorld}} -> {"State#" {RealWorld}}

  {-
    Prefetch
  -}
  primop effectful
    "prefetchByteArray3#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchMutableByteArray3#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchAddr3#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchValue3#"            :: %a -> {"State#" %s} -> {"State#" %s}
    "prefetchByteArray2#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchMutableByteArray2#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchAddr2#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchValue2#"            :: %a -> {"State#" %s} -> {"State#" %s}
    "prefetchByteArray1#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchMutableByteArray1#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchAddr1#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchValue1#"            :: %a -> {"State#" %s} -> {"State#" %s}
    "prefetchByteArray0#"        :: {"ByteArray#"} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchMutableByteArray0#" :: {"MutableByteArray#" %s} -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchAddr0#"             :: T_Addr -> T_Int64 -> {"State#" %s} -> {"State#" %s}
    "prefetchValue0#"            :: %a -> {"State#" %s} -> {"State#" %s}

  |]

unsupported :: Set String
unsupported = Set.fromList
  [ "writeOffAddrAs#"
  , "readOffAddrAs#"
  , "indexOffAddrAs#"
  , "writeArrayAs#"
  , "readArrayAs#"
  , "indexArrayAs#"
  , "writeOffAddr#"
  , "readOffAddr#"
  , "indexOffAddr#"
  , "writeArray#"
  , "readArray#"
  , "indexArray#"
  , "negate#"
  , "rem#"
  , "quot#"
  , "divide#"
  , "times#"
  , "minus#"
  , "plus#"
  , "insert#"
  , "unpack#"
  , "pack#"
  , "broadcast#"
  , "coerce"
  , "unsafeCoerce#"
  , "seq"
  , "proxy#"
  , "clearCCS#"
  , "getApStackVal#"
  , "unpackClosure#"
  , "mkApUpd0#"
  , "addrToAny#"
  , "tagToEnum#"
  , "getSpark#"
  , "finalizeWeak#"
  , "mkWeak#"
  , "newMVar#"
  , "catchSTM#"
  , "catchRetry#"
  , "retry#"
  , "atomically#"
  , "unmaskAsyncExceptions#"
  , "maskUninterruptible#"
  , "maskAsyncExceptions#"
  , "raiseIO#"
  , "raise#"
  , "catch#"
  , "atomicModifyMutVar#"
  , "readStablePtrOffAddr#"
  , "indexStablePtrOffAddr#"
  , "nullAddr#"
  , "readWord8ArrayAsStablePtr#"
  , "readStablePtrArray#"
  , "indexWord8ArrayAsStablePtr#"
  , "indexStablePtrArray#"
  ]
