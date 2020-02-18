{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Lambda.GHCPrimOps where

import qualified Data.Set as Set
import Lambda.Syntax2
import Lambda.TH

primPrelude :: Program
primPrelude = [progConst2|
  {-
    Char#
  -}
  primop pure
    "gtChar#" :: (T_Char) @ t.0 -> (T_Char) @ t.1 -> (T_Int64) @ t.2
    "geChar#" :: (T_Char) @ t.3 -> (T_Char) @ t.4 -> (T_Int64) @ t.5
    "eqChar#" :: (T_Char) @ t.6 -> (T_Char) @ t.7 -> (T_Int64) @ t.8
    "neChar#" :: (T_Char) @ t.9 -> (T_Char) @ t.10 -> (T_Int64) @ t.11
    "ltChar#" :: (T_Char) @ t.12 -> (T_Char) @ t.13 -> (T_Int64) @ t.14
    "leChar#" :: (T_Char) @ t.15 -> (T_Char) @ t.16 -> (T_Int64) @ t.17
    "ord#"    :: (T_Char) @ t.18 -> (T_Int64) @ t.19

  {-
    Int8#
  -}
  primop pure
    "extendInt8#"  :: {"Int8#"} @ t.20 -> (T_Int64) @ t.21
    "narrowInt8#"  :: (T_Int64) @ t.22 -> {"Int8#"} @ t.23
    "negateInt8#"  :: {"Int8#"} @ t.24 -> {"Int8#"} @ t.25
    "plusInt8#"    :: {"Int8#"} @ t.26 -> {"Int8#"} @ t.27 -> {"Int8#"} @ t.28
    "subInt8#"     :: {"Int8#"} @ t.29 -> {"Int8#"} @ t.30 -> {"Int8#"} @ t.31
    "timesInt8#"   :: {"Int8#"} @ t.32 -> {"Int8#"} @ t.33 -> {"Int8#"} @ t.34
    "quotInt8#"    :: {"Int8#"} @ t.35 -> {"Int8#"} @ t.36 -> {"Int8#"} @ t.37
    "remInt8#"     :: {"Int8#"} @ t.38 -> {"Int8#"} @ t.39 -> {"Int8#"} @ t.40
    "quotRemInt8#" :: {"Int8#"} @ t.41 -> {"Int8#"} @ t.42 -> {"GHC.Prim.(#,#)" {"Int8#"} @ t.43 {"Int8#"} @ t.44} @ t.45
    "eqInt8#"      :: {"Int8#"} @ t.46 -> {"Int8#"} @ t.47 -> (T_Int64) @ t.48
    "geInt8#"      :: {"Int8#"} @ t.49 -> {"Int8#"} @ t.50 -> (T_Int64) @ t.51
    "gtInt8#"      :: {"Int8#"} @ t.52 -> {"Int8#"} @ t.53 -> (T_Int64) @ t.54
    "leInt8#"      :: {"Int8#"} @ t.55 -> {"Int8#"} @ t.56 -> (T_Int64) @ t.57
    "ltInt8#"      :: {"Int8#"} @ t.58 -> {"Int8#"} @ t.59 -> (T_Int64) @ t.60
    "neInt8#"      :: {"Int8#"} @ t.61 -> {"Int8#"} @ t.62 -> (T_Int64) @ t.63

  {-
    Word8#
  -}
  primop pure
    "extendWord8#"  :: {"Word8#"} @ t.64 -> (T_Word64) @ t.65
    "narrowWord8#"  :: (T_Word64) @ t.66 -> {"Word8#"} @ t.67
    "notWord8#"     :: {"Word8#"} @ t.68 -> {"Word8#"} @ t.69
    "plusWord8#"    :: {"Word8#"} @ t.70 -> {"Word8#"} @ t.71 -> {"Word8#"} @ t.72
    "subWord8#"     :: {"Word8#"} @ t.73 -> {"Word8#"} @ t.74 -> {"Word8#"} @ t.75
    "timesWord8#"   :: {"Word8#"} @ t.76 -> {"Word8#"} @ t.77 -> {"Word8#"} @ t.78
    "quotWord8#"    :: {"Word8#"} @ t.79 -> {"Word8#"} @ t.80 -> {"Word8#"} @ t.81
    "remWord8#"     :: {"Word8#"} @ t.82 -> {"Word8#"} @ t.83 -> {"Word8#"} @ t.84
    "quotRemWord8#" :: {"Word8#"} @ t.85 -> {"Word8#"} @ t.86 -> {"GHC.Prim.(#,#)" {"Word8#"} @ t.87 {"Word8#"} @ t.88} @ t.89
    "eqWord8#"      :: {"Word8#"} @ t.90 -> {"Word8#"} @ t.91 -> (T_Int64) @ t.92
    "geWord8#"      :: {"Word8#"} @ t.93 -> {"Word8#"} @ t.94 -> (T_Int64) @ t.95
    "gtWord8#"      :: {"Word8#"} @ t.96 -> {"Word8#"} @ t.97 -> (T_Int64) @ t.98
    "leWord8#"      :: {"Word8#"} @ t.99 -> {"Word8#"} @ t.100 -> (T_Int64) @ t.101
    "ltWord8#"      :: {"Word8#"} @ t.102 -> {"Word8#"} @ t.103 -> (T_Int64) @ t.104
    "neWord8#"      :: {"Word8#"} @ t.105 -> {"Word8#"} @ t.106 -> (T_Int64) @ t.107

  {-
    Int16#
  -}
  primop pure
    "extendInt16#"  :: {"Int16#"} @ t.108 -> (T_Int64) @ t.109
    "narrowInt16#"  :: (T_Int64) @ t.110 -> {"Int16#"} @ t.111
    "negateInt16#"  :: {"Int16#"} @ t.112 -> {"Int16#"} @ t.113
    "plusInt16#"    :: {"Int16#"} @ t.114 -> {"Int16#"} @ t.115 -> {"Int16#"} @ t.116
    "subInt16#"     :: {"Int16#"} @ t.117 -> {"Int16#"} @ t.118 -> {"Int16#"} @ t.119
    "timesInt16#"   :: {"Int16#"} @ t.120 -> {"Int16#"} @ t.121 -> {"Int16#"} @ t.122
    "quotInt16#"    :: {"Int16#"} @ t.123 -> {"Int16#"} @ t.124 -> {"Int16#"} @ t.125
    "remInt16#"     :: {"Int16#"} @ t.126 -> {"Int16#"} @ t.127 -> {"Int16#"} @ t.128
    "quotRemInt16#" :: {"Int16#"} @ t.129 -> {"Int16#"} @ t.130 -> {"GHC.Prim.(#,#)" {"Int16#"} @ t.131 {"Int16#"} @ t.132} @ t.133
    "eqInt16#"      :: {"Int16#"} @ t.134 -> {"Int16#"} @ t.135 -> (T_Int64) @ t.136
    "geInt16#"      :: {"Int16#"} @ t.137 -> {"Int16#"} @ t.138 -> (T_Int64) @ t.139
    "gtInt16#"      :: {"Int16#"} @ t.140 -> {"Int16#"} @ t.141 -> (T_Int64) @ t.142
    "leInt16#"      :: {"Int16#"} @ t.143 -> {"Int16#"} @ t.144 -> (T_Int64) @ t.145
    "ltInt16#"      :: {"Int16#"} @ t.146 -> {"Int16#"} @ t.147 -> (T_Int64) @ t.148
    "neInt16#"      :: {"Int16#"} @ t.149 -> {"Int16#"} @ t.150 -> (T_Int64) @ t.151

  {-
    Word16#
  -}
  primop pure
    "extendWord16#"  :: {"Word16#"} @ t.152 -> (T_Word64) @ t.153
    "narrowWord16#"  :: (T_Word64) @ t.154 -> {"Word16#"} @ t.155
    "notWord16#"     :: {"Word16#"} @ t.156 -> {"Word16#"} @ t.157
    "plusWord16#"    :: {"Word16#"} @ t.158 -> {"Word16#"} @ t.159 -> {"Word16#"} @ t.160
    "subWord16#"     :: {"Word16#"} @ t.161 -> {"Word16#"} @ t.162 -> {"Word16#"} @ t.163
    "timesWord16#"   :: {"Word16#"} @ t.164 -> {"Word16#"} @ t.165 -> {"Word16#"} @ t.166
    "quotWord16#"    :: {"Word16#"} @ t.167 -> {"Word16#"} @ t.168 -> {"Word16#"} @ t.169
    "remWord16#"     :: {"Word16#"} @ t.170 -> {"Word16#"} @ t.171 -> {"Word16#"} @ t.172
    "quotRemWord16#" :: {"Word16#"} @ t.173 -> {"Word16#"} @ t.174 -> {"GHC.Prim.(#,#)" {"Word16#"} @ t.175 {"Word16#"} @ t.176} @ t.177
    "eqWord16#"      :: {"Word16#"} @ t.178 -> {"Word16#"} @ t.179 -> (T_Int64) @ t.180
    "geWord16#"      :: {"Word16#"} @ t.181 -> {"Word16#"} @ t.182 -> (T_Int64) @ t.183
    "gtWord16#"      :: {"Word16#"} @ t.184 -> {"Word16#"} @ t.185 -> (T_Int64) @ t.186
    "leWord16#"      :: {"Word16#"} @ t.187 -> {"Word16#"} @ t.188 -> (T_Int64) @ t.189
    "ltWord16#"      :: {"Word16#"} @ t.190 -> {"Word16#"} @ t.191 -> (T_Int64) @ t.192
    "neWord16#"      :: {"Word16#"} @ t.193 -> {"Word16#"} @ t.194 -> (T_Int64) @ t.195

  {-
    Int#
  -}
  primop pure
    "+#"                 :: (T_Int64) @ t.196 -> (T_Int64) @ t.197 -> (T_Int64) @ t.198
    "-#"                 :: (T_Int64) @ t.199 -> (T_Int64) @ t.200 -> (T_Int64) @ t.201
    "*#"                 :: (T_Int64) @ t.202 -> (T_Int64) @ t.203 -> (T_Int64) @ t.204
    "timesInt2#"         :: (T_Int64) @ t.205 -> (T_Int64) @ t.206 -> {"GHC.Prim.(#,,#)" (T_Int64) @ t.207 (T_Int64) @ t.208 (T_Int64) @ t.209} @ t.210
    "mulIntMayOflo#"     :: (T_Int64) @ t.211 -> (T_Int64) @ t.212 -> (T_Int64) @ t.213
    "quotInt#"           :: (T_Int64) @ t.214 -> (T_Int64) @ t.215 -> (T_Int64) @ t.216
    "remInt#"            :: (T_Int64) @ t.217 -> (T_Int64) @ t.218 -> (T_Int64) @ t.219
    "quotRemInt#"        :: (T_Int64) @ t.220 -> (T_Int64) @ t.221 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.222 (T_Int64) @ t.223} @ t.224
    "andI#"              :: (T_Int64) @ t.225 -> (T_Int64) @ t.226 -> (T_Int64) @ t.227
    "orI#"               :: (T_Int64) @ t.228 -> (T_Int64) @ t.229 -> (T_Int64) @ t.230
    "xorI#"              :: (T_Int64) @ t.231 -> (T_Int64) @ t.232 -> (T_Int64) @ t.233
    "notI#"              :: (T_Int64) @ t.234 -> (T_Int64) @ t.235
    "negateInt#"         :: (T_Int64) @ t.236 -> (T_Int64) @ t.237
    "addIntC#"           :: (T_Int64) @ t.238 -> (T_Int64) @ t.239 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.240 (T_Int64) @ t.241} @ t.242
    "subIntC#"           :: (T_Int64) @ t.243 -> (T_Int64) @ t.244 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.245 (T_Int64) @ t.246} @ t.247
    ">#"                 :: (T_Int64) @ t.248 -> (T_Int64) @ t.249 -> (T_Int64) @ t.250
    ">=#"                :: (T_Int64) @ t.251 -> (T_Int64) @ t.252 -> (T_Int64) @ t.253
    "==#"                :: (T_Int64) @ t.254 -> (T_Int64) @ t.255 -> (T_Int64) @ t.256
    "/=#"                :: (T_Int64) @ t.257 -> (T_Int64) @ t.258 -> (T_Int64) @ t.259
    "<#"                 :: (T_Int64) @ t.260 -> (T_Int64) @ t.261 -> (T_Int64) @ t.262
    "<=#"                :: (T_Int64) @ t.263 -> (T_Int64) @ t.264 -> (T_Int64) @ t.265
    "chr#"               :: (T_Int64) @ t.266 -> (T_Char) @ t.267
    "int2Word#"          :: (T_Int64) @ t.268 -> (T_Word64) @ t.269
    "int2Float#"         :: (T_Int64) @ t.270 -> (T_Float) @ t.271
    "int2Double#"        :: (T_Int64) @ t.272 -> (T_Double) @ t.273
    "word2Float#"        :: (T_Word64) @ t.274 -> (T_Float) @ t.275
    "word2Double#"       :: (T_Word64) @ t.276 -> (T_Double) @ t.277
    "uncheckedIShiftL#"  :: (T_Int64) @ t.278 -> (T_Int64) @ t.279 -> (T_Int64) @ t.280
    "uncheckedIShiftRA#" :: (T_Int64) @ t.281 -> (T_Int64) @ t.282 -> (T_Int64) @ t.283
    "uncheckedIShiftRL#" :: (T_Int64) @ t.284 -> (T_Int64) @ t.285 -> (T_Int64) @ t.286

  {-
    Word#
  -}
  primop pure
    "plusWord#"         :: (T_Word64) @ t.287 -> (T_Word64) @ t.288 -> (T_Word64) @ t.289
    "addWordC#"         :: (T_Word64) @ t.290 -> (T_Word64) @ t.291 -> {"GHC.Prim.(#,#)" (T_Word64) @ t.292 (T_Int64) @ t.293} @ t.294
    "subWordC#"         :: (T_Word64) @ t.295 -> (T_Word64) @ t.296 -> {"GHC.Prim.(#,#)" (T_Word64) @ t.297 (T_Int64) @ t.298} @ t.299
    "plusWord2#"        :: (T_Word64) @ t.300 -> (T_Word64) @ t.301 -> {"GHC.Prim.(#,#)" (T_Word64) @ t.302 (T_Word64) @ t.303} @ t.304
    "minusWord#"        :: (T_Word64) @ t.305 -> (T_Word64) @ t.306 -> (T_Word64) @ t.307
    "timesWord#"        :: (T_Word64) @ t.308 -> (T_Word64) @ t.309 -> (T_Word64) @ t.310
    "timesWord2#"       :: (T_Word64) @ t.311 -> (T_Word64) @ t.312 -> {"GHC.Prim.(#,#)" (T_Word64) @ t.313 (T_Word64) @ t.314} @ t.315
    "quotWord#"         :: (T_Word64) @ t.316 -> (T_Word64) @ t.317 -> (T_Word64) @ t.318
    "remWord#"          :: (T_Word64) @ t.319 -> (T_Word64) @ t.320 -> (T_Word64) @ t.321
    "quotRemWord#"      :: (T_Word64) @ t.322 -> (T_Word64) @ t.323 -> {"GHC.Prim.(#,#)" (T_Word64) @ t.324 (T_Word64) @ t.325} @ t.326
    "quotRemWord2#"     :: (T_Word64) @ t.327 -> (T_Word64) @ t.328 -> (T_Word64) @ t.329 -> {"GHC.Prim.(#,#)" (T_Word64) @ t.330 (T_Word64) @ t.331} @ t.332
    "and#"              :: (T_Word64) @ t.333 -> (T_Word64) @ t.334 -> (T_Word64) @ t.335
    "or#"               :: (T_Word64) @ t.336 -> (T_Word64) @ t.337 -> (T_Word64) @ t.338
    "xor#"              :: (T_Word64) @ t.339 -> (T_Word64) @ t.340 -> (T_Word64) @ t.341
    "not#"              :: (T_Word64) @ t.342 -> (T_Word64) @ t.343
    "uncheckedShiftL#"  :: (T_Word64) @ t.344 -> (T_Int64) @ t.345 -> (T_Word64) @ t.346
    "uncheckedShiftRL#" :: (T_Word64) @ t.347 -> (T_Int64) @ t.348 -> (T_Word64) @ t.349
    "word2Int#"         :: (T_Word64) @ t.350 -> (T_Int64) @ t.351
    "gtWord#"           :: (T_Word64) @ t.352 -> (T_Word64) @ t.353 -> (T_Int64) @ t.354
    "geWord#"           :: (T_Word64) @ t.355 -> (T_Word64) @ t.356 -> (T_Int64) @ t.357
    "eqWord#"           :: (T_Word64) @ t.358 -> (T_Word64) @ t.359 -> (T_Int64) @ t.360
    "neWord#"           :: (T_Word64) @ t.361 -> (T_Word64) @ t.362 -> (T_Int64) @ t.363
    "ltWord#"           :: (T_Word64) @ t.364 -> (T_Word64) @ t.365 -> (T_Int64) @ t.366
    "leWord#"           :: (T_Word64) @ t.367 -> (T_Word64) @ t.368 -> (T_Int64) @ t.369
    "popCnt8#"          :: (T_Word64) @ t.370 -> (T_Word64) @ t.371
    "popCnt16#"         :: (T_Word64) @ t.372 -> (T_Word64) @ t.373
    "popCnt32#"         :: (T_Word64) @ t.374 -> (T_Word64) @ t.375
    "popCnt64#"         :: (T_Word64) @ t.376 -> (T_Word64) @ t.377
    "popCnt#"           :: (T_Word64) @ t.378 -> (T_Word64) @ t.379
    "pdep8#"            :: (T_Word64) @ t.380 -> (T_Word64) @ t.381 -> (T_Word64) @ t.382
    "pdep16#"           :: (T_Word64) @ t.383 -> (T_Word64) @ t.384 -> (T_Word64) @ t.385
    "pdep32#"           :: (T_Word64) @ t.386 -> (T_Word64) @ t.387 -> (T_Word64) @ t.388
    "pdep64#"           :: (T_Word64) @ t.389 -> (T_Word64) @ t.390 -> (T_Word64) @ t.391
    "pdep#"             :: (T_Word64) @ t.392 -> (T_Word64) @ t.393 -> (T_Word64) @ t.394
    "pext8#"            :: (T_Word64) @ t.395 -> (T_Word64) @ t.396 -> (T_Word64) @ t.397
    "pext16#"           :: (T_Word64) @ t.398 -> (T_Word64) @ t.399 -> (T_Word64) @ t.400
    "pext32#"           :: (T_Word64) @ t.401 -> (T_Word64) @ t.402 -> (T_Word64) @ t.403
    "pext64#"           :: (T_Word64) @ t.404 -> (T_Word64) @ t.405 -> (T_Word64) @ t.406
    "pext#"             :: (T_Word64) @ t.407 -> (T_Word64) @ t.408 -> (T_Word64) @ t.409
    "clz8#"             :: (T_Word64) @ t.410 -> (T_Word64) @ t.411
    "clz16#"            :: (T_Word64) @ t.412 -> (T_Word64) @ t.413
    "clz32#"            :: (T_Word64) @ t.414 -> (T_Word64) @ t.415
    "clz64#"            :: (T_Word64) @ t.416 -> (T_Word64) @ t.417
    "clz#"              :: (T_Word64) @ t.418 -> (T_Word64) @ t.419
    "ctz8#"             :: (T_Word64) @ t.420 -> (T_Word64) @ t.421
    "ctz16#"            :: (T_Word64) @ t.422 -> (T_Word64) @ t.423
    "ctz32#"            :: (T_Word64) @ t.424 -> (T_Word64) @ t.425
    "ctz64#"            :: (T_Word64) @ t.426 -> (T_Word64) @ t.427
    "ctz#"              :: (T_Word64) @ t.428 -> (T_Word64) @ t.429
    "byteSwap16#"       :: (T_Word64) @ t.430 -> (T_Word64) @ t.431
    "byteSwap32#"       :: (T_Word64) @ t.432 -> (T_Word64) @ t.433
    "byteSwap64#"       :: (T_Word64) @ t.434 -> (T_Word64) @ t.435
    "byteSwap#"         :: (T_Word64) @ t.436 -> (T_Word64) @ t.437
    "bitReverse8#"      :: (T_Word64) @ t.438 -> (T_Word64) @ t.439
    "bitReverse16#"     :: (T_Word64) @ t.440 -> (T_Word64) @ t.441
    "bitReverse32#"     :: (T_Word64) @ t.442 -> (T_Word64) @ t.443
    "bitReverse64#"     :: (T_Word64) @ t.444 -> (T_Word64) @ t.445
    "bitReverse#"       :: (T_Word64) @ t.446 -> (T_Word64) @ t.447

  {-
    Narrowings
  -}
  primop pure
    "narrow8Int#"   :: (T_Int64) @ t.448 -> (T_Int64) @ t.449
    "narrow16Int#"  :: (T_Int64) @ t.450 -> (T_Int64) @ t.451
    "narrow32Int#"  :: (T_Int64) @ t.452 -> (T_Int64) @ t.453
    "narrow8Word#"  :: (T_Word64) @ t.454 -> (T_Word64) @ t.455
    "narrow16Word#" :: (T_Word64) @ t.456 -> (T_Word64) @ t.457
    "narrow32Word#" :: (T_Word64) @ t.458 -> (T_Word64) @ t.459

  {-
    Double#
  -}
  primop pure
    ">##"                 :: (T_Double) @ t.460 -> (T_Double) @ t.461 -> (T_Int64) @ t.462
    ">=##"                :: (T_Double) @ t.463 -> (T_Double) @ t.464 -> (T_Int64) @ t.465
    "==##"                :: (T_Double) @ t.466 -> (T_Double) @ t.467 -> (T_Int64) @ t.468
    "/=##"                :: (T_Double) @ t.469 -> (T_Double) @ t.470 -> (T_Int64) @ t.471
    "<##"                 :: (T_Double) @ t.472 -> (T_Double) @ t.473 -> (T_Int64) @ t.474
    "<=##"                :: (T_Double) @ t.475 -> (T_Double) @ t.476 -> (T_Int64) @ t.477
    "+##"                 :: (T_Double) @ t.478 -> (T_Double) @ t.479 -> (T_Double) @ t.480
    "-##"                 :: (T_Double) @ t.481 -> (T_Double) @ t.482 -> (T_Double) @ t.483
    "*##"                 :: (T_Double) @ t.484 -> (T_Double) @ t.485 -> (T_Double) @ t.486
    "/##"                 :: (T_Double) @ t.487 -> (T_Double) @ t.488 -> (T_Double) @ t.489
    "negateDouble#"       :: (T_Double) @ t.490 -> (T_Double) @ t.491
    "fabsDouble#"         :: (T_Double) @ t.492 -> (T_Double) @ t.493
    "double2Int#"         :: (T_Double) @ t.494 -> (T_Int64) @ t.495
    "double2Float#"       :: (T_Double) @ t.496 -> (T_Float) @ t.497
    "expDouble#"          :: (T_Double) @ t.498 -> (T_Double) @ t.499
    "expm1Double#"        :: (T_Double) @ t.500 -> (T_Double) @ t.501
    "logDouble#"          :: (T_Double) @ t.502 -> (T_Double) @ t.503
    "log1pDouble#"        :: (T_Double) @ t.504 -> (T_Double) @ t.505
    "sqrtDouble#"         :: (T_Double) @ t.506 -> (T_Double) @ t.507
    "sinDouble#"          :: (T_Double) @ t.508 -> (T_Double) @ t.509
    "cosDouble#"          :: (T_Double) @ t.510 -> (T_Double) @ t.511
    "tanDouble#"          :: (T_Double) @ t.512 -> (T_Double) @ t.513
    "asinDouble#"         :: (T_Double) @ t.514 -> (T_Double) @ t.515
    "acosDouble#"         :: (T_Double) @ t.516 -> (T_Double) @ t.517
    "atanDouble#"         :: (T_Double) @ t.518 -> (T_Double) @ t.519
    "sinhDouble#"         :: (T_Double) @ t.520 -> (T_Double) @ t.521
    "coshDouble#"         :: (T_Double) @ t.522 -> (T_Double) @ t.523
    "tanhDouble#"         :: (T_Double) @ t.524 -> (T_Double) @ t.525
    "asinhDouble#"        :: (T_Double) @ t.526 -> (T_Double) @ t.527
    "acoshDouble#"        :: (T_Double) @ t.528 -> (T_Double) @ t.529
    "atanhDouble#"        :: (T_Double) @ t.530 -> (T_Double) @ t.531
    "**##"                :: (T_Double) @ t.532 -> (T_Double) @ t.533 -> (T_Double) @ t.534
    "decodeDouble_2Int#"  :: (T_Double) @ t.535 -> {"GHC.Prim.(#,,,#)" (T_Int64) @ t.536 (T_Word64) @ t.537 (T_Word64) @ t.538 (T_Int64) @ t.539} @ t.540
    "decodeDouble_Int64#" :: (T_Double) @ t.541 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.542 (T_Int64) @ t.543} @ t.544

  {-
    Float#
  -}
  primop pure
    "gtFloat#"         :: (T_Float) @ t.545 -> (T_Float) @ t.546 -> (T_Int64) @ t.547
    "geFloat#"         :: (T_Float) @ t.548 -> (T_Float) @ t.549 -> (T_Int64) @ t.550
    "eqFloat#"         :: (T_Float) @ t.551 -> (T_Float) @ t.552 -> (T_Int64) @ t.553
    "neFloat#"         :: (T_Float) @ t.554 -> (T_Float) @ t.555 -> (T_Int64) @ t.556
    "ltFloat#"         :: (T_Float) @ t.557 -> (T_Float) @ t.558 -> (T_Int64) @ t.559
    "leFloat#"         :: (T_Float) @ t.560 -> (T_Float) @ t.561 -> (T_Int64) @ t.562
    "plusFloat#"       :: (T_Float) @ t.563 -> (T_Float) @ t.564 -> (T_Float) @ t.565
    "minusFloat#"      :: (T_Float) @ t.566 -> (T_Float) @ t.567 -> (T_Float) @ t.568
    "timesFloat#"      :: (T_Float) @ t.569 -> (T_Float) @ t.570 -> (T_Float) @ t.571
    "divideFloat#"     :: (T_Float) @ t.572 -> (T_Float) @ t.573 -> (T_Float) @ t.574
    "negateFloat#"     :: (T_Float) @ t.575 -> (T_Float) @ t.576
    "fabsFloat#"       :: (T_Float) @ t.577 -> (T_Float) @ t.578
    "float2Int#"       :: (T_Float) @ t.579 -> (T_Int64) @ t.580
    "expFloat#"        :: (T_Float) @ t.581 -> (T_Float) @ t.582
    "expm1Float#"      :: (T_Float) @ t.583 -> (T_Float) @ t.584
    "logFloat#"        :: (T_Float) @ t.585 -> (T_Float) @ t.586
    "log1pFloat#"      :: (T_Float) @ t.587 -> (T_Float) @ t.588
    "sqrtFloat#"       :: (T_Float) @ t.589 -> (T_Float) @ t.590
    "sinFloat#"        :: (T_Float) @ t.591 -> (T_Float) @ t.592
    "cosFloat#"        :: (T_Float) @ t.593 -> (T_Float) @ t.594
    "tanFloat#"        :: (T_Float) @ t.595 -> (T_Float) @ t.596
    "asinFloat#"       :: (T_Float) @ t.597 -> (T_Float) @ t.598
    "acosFloat#"       :: (T_Float) @ t.599 -> (T_Float) @ t.600
    "atanFloat#"       :: (T_Float) @ t.601 -> (T_Float) @ t.602
    "sinhFloat#"       :: (T_Float) @ t.603 -> (T_Float) @ t.604
    "coshFloat#"       :: (T_Float) @ t.605 -> (T_Float) @ t.606
    "tanhFloat#"       :: (T_Float) @ t.607 -> (T_Float) @ t.608
    "asinhFloat#"      :: (T_Float) @ t.609 -> (T_Float) @ t.610
    "acoshFloat#"      :: (T_Float) @ t.611 -> (T_Float) @ t.612
    "atanhFloat#"      :: (T_Float) @ t.613 -> (T_Float) @ t.614
    "powerFloat#"      :: (T_Float) @ t.615 -> (T_Float) @ t.616 -> (T_Float) @ t.617
    "float2Double#"    :: (T_Float) @ t.618 -> (T_Double) @ t.619
    "decodeFloat_Int#" :: (T_Float) @ t.620 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.621 (T_Int64) @ t.622} @ t.623

  {-
    Arrays
  -}
  primop effectful
    "newArray#" :: (T_Int64) @ t.624 -> %a.0 -> {"State#" %s.0} @ t.625 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.0 %a.0} @ t.626} @ t.627

  primop pure
    "sameMutableArray#" :: {"MutableArray#" %s.1 %a.1} @ t.628 -> {"MutableArray#" %s.1 %a.1} @ t.629 -> (T_Int64) @ t.630

  primop effectful
    "readArray#"  :: {"MutableArray#" %s.2 %a.2} @ t.631 -> (T_Int64) @ t.632 -> {"State#" %s.2} @ t.633 -> {"GHC.Prim.Unit#" %a.2} @ t.634
    "writeArray#" :: {"MutableArray#" %s.3 %a.3} @ t.635 -> (T_Int64) @ t.636 -> %a.3 -> {"State#" %s.3} @ t.637 -> {"GHC.Prim.(##)"} @ t.638

  primop pure
    "sizeofArray#"        :: {"Array#" %a.4} @ t.639 -> (T_Int64) @ t.640
    "sizeofMutableArray#" :: {"MutableArray#" %s.4 %a.5} @ t.641 -> (T_Int64) @ t.642
    "indexArray#"         :: {"Array#" %a.6} @ t.643 -> (T_Int64) @ t.644 -> {"GHC.Prim.Unit#" %a.6} @ t.645

  primop effectful
    "unsafeFreezeArray#" :: {"MutableArray#" %s.5 %a.7} @ t.646 -> {"State#" %s.5} @ t.647 -> {"GHC.Prim.Unit#" {"Array#" %a.7} @ t.648} @ t.649
    "unsafeThawArray#"   :: {"Array#" %a.8} @ t.650 -> {"State#" %s.6} @ t.651 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.6 %a.8} @ t.652} @ t.653
    "copyArray#"         :: {"Array#" %a.9} @ t.654 -> (T_Int64) @ t.655 -> {"MutableArray#" %s.7 %a.9} @ t.656 -> (T_Int64) @ t.657 -> (T_Int64) @ t.658 -> {"State#" %s.7} @ t.659 -> {"GHC.Prim.(##)"} @ t.660
    "copyMutableArray#"  :: {"MutableArray#" %s.8 %a.10} @ t.661 -> (T_Int64) @ t.662 -> {"MutableArray#" %s.8 %a.10} @ t.663 -> (T_Int64) @ t.664 -> (T_Int64) @ t.665 -> {"State#" %s.8} @ t.666 -> {"GHC.Prim.(##)"} @ t.667
    "cloneArray#"        :: {"Array#" %a.11} @ t.668 -> (T_Int64) @ t.669 -> (T_Int64) @ t.670 -> {"Array#" %a.11} @ t.671
    "cloneMutableArray#" :: {"MutableArray#" %s.9 %a.12} @ t.672 -> (T_Int64) @ t.673 -> (T_Int64) @ t.674 -> {"State#" %s.9} @ t.675 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.9 %a.12} @ t.676} @ t.677
    "freezeArray#"       :: {"MutableArray#" %s.10 %a.13} @ t.678 -> (T_Int64) @ t.679 -> (T_Int64) @ t.680 -> {"State#" %s.10} @ t.681 -> {"GHC.Prim.Unit#" {"Array#" %a.13} @ t.682} @ t.683
    "thawArray#"         :: {"Array#" %a.14} @ t.684 -> (T_Int64) @ t.685 -> (T_Int64) @ t.686 -> {"State#" %s.11} @ t.687 -> {"GHC.Prim.Unit#" {"MutableArray#" %s.11 %a.14} @ t.688} @ t.689
    "casArray#"          :: {"MutableArray#" %s.12 %a.15} @ t.690 -> (T_Int64) @ t.691 -> %a.15 -> %a.15 -> {"State#" %s.12} @ t.692 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.693 %a.15} @ t.694

  {-
    Small Arrays
  -}
  primop effectful
    "newSmallArray#" :: (T_Int64) @ t.695 -> %a.16 -> {"State#" %s.13} @ t.696 -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s.13 %a.16} @ t.697} @ t.698

  primop pure
    "sameSmallMutableArray#" :: {"SmallMutableArray#" %s.14 %a.17} @ t.699 -> {"SmallMutableArray#" %s.14 %a.17} @ t.700 -> (T_Int64) @ t.701

  primop effectful
    "shrinkSmallMutableArray#" :: {"SmallMutableArray#" %s.15 %a.18} @ t.702 -> (T_Int64) @ t.703 -> {"State#" %s.15} @ t.704 -> {"GHC.Prim.(##)"} @ t.705
    "readSmallArray#"          :: {"SmallMutableArray#" %s.16 %a.19} @ t.706 -> (T_Int64) @ t.707 -> {"State#" %s.16} @ t.708 -> {"GHC.Prim.Unit#" %a.19} @ t.709
    "writeSmallArray#"         :: {"SmallMutableArray#" %s.17 %a.20} @ t.710 -> (T_Int64) @ t.711 -> %a.20 -> {"State#" %s.17} @ t.712 -> {"GHC.Prim.(##)"} @ t.713

  primop pure
    "sizeofSmallArray#"           :: {"SmallArray#" %a.21} @ t.714 -> (T_Int64) @ t.715
    "sizeofSmallMutableArray#"    :: {"SmallMutableArray#" %s.18 %a.22} @ t.716 -> (T_Int64) @ t.717
    "getSizeofSmallMutableArray#" :: {"SmallMutableArray#" %s.19 %a.23} @ t.718 -> {"State#" %s.19} @ t.719 -> {"GHC.Prim.Unit#" (T_Int64) @ t.720} @ t.721
    "indexSmallArray#"            :: {"SmallArray#" %a.24} @ t.722 -> (T_Int64) @ t.723 -> {"GHC.Prim.Unit#" %a.24} @ t.724

  primop effectful
    "unsafeFreezeSmallArray#" :: {"SmallMutableArray#" %s.20 %a.25} @ t.725 -> {"State#" %s.20} @ t.726 -> {"GHC.Prim.Unit#" {"SmallArray#" %a.25} @ t.727} @ t.728
    "unsafeThawSmallArray#"   :: {"SmallArray#" %a.26} @ t.729 -> {"State#" %s.21} @ t.730 -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s.21 %a.26} @ t.731} @ t.732
    "copySmallArray#"         :: {"SmallArray#" %a.27} @ t.733 -> (T_Int64) @ t.734 -> {"SmallMutableArray#" %s.22 %a.27} @ t.735 -> (T_Int64) @ t.736 -> (T_Int64) @ t.737 -> {"State#" %s.22} @ t.738 -> {"GHC.Prim.(##)"} @ t.739
    "copySmallMutableArray#"  :: {"SmallMutableArray#" %s.23 %a.28} @ t.740 -> (T_Int64) @ t.741 -> {"SmallMutableArray#" %s.23 %a.28} @ t.742 -> (T_Int64) @ t.743 -> (T_Int64) @ t.744 -> {"State#" %s.23} @ t.745 -> {"GHC.Prim.(##)"} @ t.746
    "cloneSmallArray#"        :: {"SmallArray#" %a.29} @ t.747 -> (T_Int64) @ t.748 -> (T_Int64) @ t.749 -> {"SmallArray#" %a.29} @ t.750
    "cloneSmallMutableArray#" :: {"SmallMutableArray#" %s.24 %a.30} @ t.751 -> (T_Int64) @ t.752 -> (T_Int64) @ t.753 -> {"State#" %s.24} @ t.754 -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s.24 %a.30} @ t.755} @ t.756
    "freezeSmallArray#"       :: {"SmallMutableArray#" %s.25 %a.31} @ t.757 -> (T_Int64) @ t.758 -> (T_Int64) @ t.759 -> {"State#" %s.25} @ t.760 -> {"GHC.Prim.Unit#" {"SmallArray#" %a.31} @ t.761} @ t.762
    "thawSmallArray#"         :: {"SmallArray#" %a.32} @ t.763 -> (T_Int64) @ t.764 -> (T_Int64) @ t.765 -> {"State#" %s.26} @ t.766 -> {"GHC.Prim.Unit#" {"SmallMutableArray#" %s.26 %a.32} @ t.767} @ t.768
    "casSmallArray#"          :: {"SmallMutableArray#" %s.27 %a.33} @ t.769 -> (T_Int64) @ t.770 -> %a.33 -> %a.33 -> {"State#" %s.27} @ t.771 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.772 %a.33} @ t.773

  {-
    Byte Arrays
  -}
  primop effectful
    "newByteArray#"              :: (T_Int64) @ t.774 -> {"State#" %s.28} @ t.775 -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s.28} @ t.776} @ t.777
    "newPinnedByteArray#"        :: (T_Int64) @ t.778 -> {"State#" %s.29} @ t.779 -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s.29} @ t.780} @ t.781
    "newAlignedPinnedByteArray#" :: (T_Int64) @ t.782 -> (T_Int64) @ t.783 -> {"State#" %s.30} @ t.784 -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s.30} @ t.785} @ t.786

  primop pure
    "isMutableByteArrayPinned#" :: {"MutableByteArray#" %s.31} @ t.787 -> (T_Int64) @ t.788
    "isByteArrayPinned#"        :: {"ByteArray#"} @ t.789 -> (T_Int64) @ t.790
    "byteArrayContents#"        :: {"ByteArray#"} @ t.791 -> (T_Addr) @ t.792
    "sameMutableByteArray#"     :: {"MutableByteArray#" %s.32} @ t.793 -> {"MutableByteArray#" %s.32} @ t.794 -> (T_Int64) @ t.795

  primop effectful
    "shrinkMutableByteArray#" :: {"MutableByteArray#" %s.33} @ t.796 -> (T_Int64) @ t.797 -> {"State#" %s.33} @ t.798 -> {"GHC.Prim.(##)"} @ t.799
    "resizeMutableByteArray#" :: {"MutableByteArray#" %s.34} @ t.800 -> (T_Int64) @ t.801 -> {"State#" %s.34} @ t.802 -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s.34} @ t.803} @ t.804
    "unsafeFreezeByteArray#"  :: {"MutableByteArray#" %s.35} @ t.805 -> {"State#" %s.35} @ t.806 -> {"GHC.Prim.Unit#" {"ByteArray#"} @ t.807} @ t.808

  primop pure
    "sizeofByteArray#"            :: {"ByteArray#"} @ t.809 -> (T_Int64) @ t.810
    "sizeofMutableByteArray#"     :: {"MutableByteArray#" %s.36} @ t.811 -> (T_Int64) @ t.812
    "getSizeofMutableByteArray#"  :: {"MutableByteArray#" %s.37} @ t.813 -> {"State#" %s.37} @ t.814 -> {"GHC.Prim.Unit#" (T_Int64) @ t.815} @ t.816
    "indexCharArray#"             :: {"ByteArray#"} @ t.817 -> (T_Int64) @ t.818 -> (T_Char) @ t.819
    "indexWideCharArray#"         :: {"ByteArray#"} @ t.820 -> (T_Int64) @ t.821 -> (T_Char) @ t.822
    "indexIntArray#"              :: {"ByteArray#"} @ t.823 -> (T_Int64) @ t.824 -> (T_Int64) @ t.825
    "indexWordArray#"             :: {"ByteArray#"} @ t.826 -> (T_Int64) @ t.827 -> (T_Word64) @ t.828
    "indexAddrArray#"             :: {"ByteArray#"} @ t.829 -> (T_Int64) @ t.830 -> (T_Addr) @ t.831
    "indexFloatArray#"            :: {"ByteArray#"} @ t.832 -> (T_Int64) @ t.833 -> (T_Float) @ t.834
    "indexDoubleArray#"           :: {"ByteArray#"} @ t.835 -> (T_Int64) @ t.836 -> (T_Double) @ t.837
    "indexStablePtrArray#"        :: {"ByteArray#"} @ t.838 -> (T_Int64) @ t.839 -> {"StablePtr#" %a.34} @ t.840
    "indexInt8Array#"             :: {"ByteArray#"} @ t.841 -> (T_Int64) @ t.842 -> (T_Int64) @ t.843
    "indexInt16Array#"            :: {"ByteArray#"} @ t.844 -> (T_Int64) @ t.845 -> (T_Int64) @ t.846
    "indexInt32Array#"            :: {"ByteArray#"} @ t.847 -> (T_Int64) @ t.848 -> (T_Int64) @ t.849
    "indexInt64Array#"            :: {"ByteArray#"} @ t.850 -> (T_Int64) @ t.851 -> (T_Int64) @ t.852
    "indexWord8Array#"            :: {"ByteArray#"} @ t.853 -> (T_Int64) @ t.854 -> (T_Word64) @ t.855
    "indexWord16Array#"           :: {"ByteArray#"} @ t.856 -> (T_Int64) @ t.857 -> (T_Word64) @ t.858
    "indexWord32Array#"           :: {"ByteArray#"} @ t.859 -> (T_Int64) @ t.860 -> (T_Word64) @ t.861
    "indexWord64Array#"           :: {"ByteArray#"} @ t.862 -> (T_Int64) @ t.863 -> (T_Word64) @ t.864
    "indexWord8ArrayAsChar#"      :: {"ByteArray#"} @ t.865 -> (T_Int64) @ t.866 -> (T_Char) @ t.867
    "indexWord8ArrayAsWideChar#"  :: {"ByteArray#"} @ t.868 -> (T_Int64) @ t.869 -> (T_Char) @ t.870
    "indexWord8ArrayAsAddr#"      :: {"ByteArray#"} @ t.871 -> (T_Int64) @ t.872 -> (T_Addr) @ t.873
    "indexWord8ArrayAsFloat#"     :: {"ByteArray#"} @ t.874 -> (T_Int64) @ t.875 -> (T_Float) @ t.876
    "indexWord8ArrayAsDouble#"    :: {"ByteArray#"} @ t.877 -> (T_Int64) @ t.878 -> (T_Double) @ t.879
    "indexWord8ArrayAsStablePtr#" :: {"ByteArray#"} @ t.880 -> (T_Int64) @ t.881 -> {"StablePtr#" %a.35} @ t.882
    "indexWord8ArrayAsInt16#"     :: {"ByteArray#"} @ t.883 -> (T_Int64) @ t.884 -> (T_Int64) @ t.885
    "indexWord8ArrayAsInt32#"     :: {"ByteArray#"} @ t.886 -> (T_Int64) @ t.887 -> (T_Int64) @ t.888
    "indexWord8ArrayAsInt64#"     :: {"ByteArray#"} @ t.889 -> (T_Int64) @ t.890 -> (T_Int64) @ t.891
    "indexWord8ArrayAsInt#"       :: {"ByteArray#"} @ t.892 -> (T_Int64) @ t.893 -> (T_Int64) @ t.894
    "indexWord8ArrayAsWord16#"    :: {"ByteArray#"} @ t.895 -> (T_Int64) @ t.896 -> (T_Word64) @ t.897
    "indexWord8ArrayAsWord32#"    :: {"ByteArray#"} @ t.898 -> (T_Int64) @ t.899 -> (T_Word64) @ t.900
    "indexWord8ArrayAsWord64#"    :: {"ByteArray#"} @ t.901 -> (T_Int64) @ t.902 -> (T_Word64) @ t.903
    "indexWord8ArrayAsWord#"      :: {"ByteArray#"} @ t.904 -> (T_Int64) @ t.905 -> (T_Word64) @ t.906

  primop effectful
    "readCharArray#"              :: {"MutableByteArray#" %s.38} @ t.907 -> (T_Int64) @ t.908 -> {"State#" %s.38} @ t.909 -> {"GHC.Prim.Unit#" (T_Char) @ t.910} @ t.911
    "readWideCharArray#"          :: {"MutableByteArray#" %s.39} @ t.912 -> (T_Int64) @ t.913 -> {"State#" %s.39} @ t.914 -> {"GHC.Prim.Unit#" (T_Char) @ t.915} @ t.916
    "readIntArray#"               :: {"MutableByteArray#" %s.40} @ t.917 -> (T_Int64) @ t.918 -> {"State#" %s.40} @ t.919 -> {"GHC.Prim.Unit#" (T_Int64) @ t.920} @ t.921
    "readWordArray#"              :: {"MutableByteArray#" %s.41} @ t.922 -> (T_Int64) @ t.923 -> {"State#" %s.41} @ t.924 -> {"GHC.Prim.Unit#" (T_Word64) @ t.925} @ t.926
    "readAddrArray#"              :: {"MutableByteArray#" %s.42} @ t.927 -> (T_Int64) @ t.928 -> {"State#" %s.42} @ t.929 -> {"GHC.Prim.Unit#" (T_Addr) @ t.930} @ t.931
    "readFloatArray#"             :: {"MutableByteArray#" %s.43} @ t.932 -> (T_Int64) @ t.933 -> {"State#" %s.43} @ t.934 -> {"GHC.Prim.Unit#" (T_Float) @ t.935} @ t.936
    "readDoubleArray#"            :: {"MutableByteArray#" %s.44} @ t.937 -> (T_Int64) @ t.938 -> {"State#" %s.44} @ t.939 -> {"GHC.Prim.Unit#" (T_Double) @ t.940} @ t.941
    "readStablePtrArray#"         :: {"MutableByteArray#" %s.45} @ t.942 -> (T_Int64) @ t.943 -> {"State#" %s.45} @ t.944 -> {"GHC.Prim.Unit#" {"StablePtr#" %a.36} @ t.945} @ t.946
    "readInt8Array#"              :: {"MutableByteArray#" %s.46} @ t.947 -> (T_Int64) @ t.948 -> {"State#" %s.46} @ t.949 -> {"GHC.Prim.Unit#" (T_Int64) @ t.950} @ t.951
    "readInt16Array#"             :: {"MutableByteArray#" %s.47} @ t.952 -> (T_Int64) @ t.953 -> {"State#" %s.47} @ t.954 -> {"GHC.Prim.Unit#" (T_Int64) @ t.955} @ t.956
    "readInt32Array#"             :: {"MutableByteArray#" %s.48} @ t.957 -> (T_Int64) @ t.958 -> {"State#" %s.48} @ t.959 -> {"GHC.Prim.Unit#" (T_Int64) @ t.960} @ t.961
    "readInt64Array#"             :: {"MutableByteArray#" %s.49} @ t.962 -> (T_Int64) @ t.963 -> {"State#" %s.49} @ t.964 -> {"GHC.Prim.Unit#" (T_Int64) @ t.965} @ t.966
    "readWord8Array#"             :: {"MutableByteArray#" %s.50} @ t.967 -> (T_Int64) @ t.968 -> {"State#" %s.50} @ t.969 -> {"GHC.Prim.Unit#" (T_Word64) @ t.970} @ t.971
    "readWord16Array#"            :: {"MutableByteArray#" %s.51} @ t.972 -> (T_Int64) @ t.973 -> {"State#" %s.51} @ t.974 -> {"GHC.Prim.Unit#" (T_Word64) @ t.975} @ t.976
    "readWord32Array#"            :: {"MutableByteArray#" %s.52} @ t.977 -> (T_Int64) @ t.978 -> {"State#" %s.52} @ t.979 -> {"GHC.Prim.Unit#" (T_Word64) @ t.980} @ t.981
    "readWord64Array#"            :: {"MutableByteArray#" %s.53} @ t.982 -> (T_Int64) @ t.983 -> {"State#" %s.53} @ t.984 -> {"GHC.Prim.Unit#" (T_Word64) @ t.985} @ t.986
    "readWord8ArrayAsChar#"       :: {"MutableByteArray#" %s.54} @ t.987 -> (T_Int64) @ t.988 -> {"State#" %s.54} @ t.989 -> {"GHC.Prim.Unit#" (T_Char) @ t.990} @ t.991
    "readWord8ArrayAsWideChar#"   :: {"MutableByteArray#" %s.55} @ t.992 -> (T_Int64) @ t.993 -> {"State#" %s.55} @ t.994 -> {"GHC.Prim.Unit#" (T_Char) @ t.995} @ t.996
    "readWord8ArrayAsAddr#"       :: {"MutableByteArray#" %s.56} @ t.997 -> (T_Int64) @ t.998 -> {"State#" %s.56} @ t.999 -> {"GHC.Prim.Unit#" (T_Addr) @ t.1000} @ t.1001
    "readWord8ArrayAsFloat#"      :: {"MutableByteArray#" %s.57} @ t.1002 -> (T_Int64) @ t.1003 -> {"State#" %s.57} @ t.1004 -> {"GHC.Prim.Unit#" (T_Float) @ t.1005} @ t.1006
    "readWord8ArrayAsDouble#"     :: {"MutableByteArray#" %s.58} @ t.1007 -> (T_Int64) @ t.1008 -> {"State#" %s.58} @ t.1009 -> {"GHC.Prim.Unit#" (T_Double) @ t.1010} @ t.1011
    "readWord8ArrayAsStablePtr#"  :: {"MutableByteArray#" %s.59} @ t.1012 -> (T_Int64) @ t.1013 -> {"State#" %s.59} @ t.1014 -> {"GHC.Prim.Unit#" {"StablePtr#" %a.37} @ t.1015} @ t.1016
    "readWord8ArrayAsInt16#"      :: {"MutableByteArray#" %s.60} @ t.1017 -> (T_Int64) @ t.1018 -> {"State#" %s.60} @ t.1019 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1020} @ t.1021
    "readWord8ArrayAsInt32#"      :: {"MutableByteArray#" %s.61} @ t.1022 -> (T_Int64) @ t.1023 -> {"State#" %s.61} @ t.1024 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1025} @ t.1026
    "readWord8ArrayAsInt64#"      :: {"MutableByteArray#" %s.62} @ t.1027 -> (T_Int64) @ t.1028 -> {"State#" %s.62} @ t.1029 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1030} @ t.1031
    "readWord8ArrayAsInt#"        :: {"MutableByteArray#" %s.63} @ t.1032 -> (T_Int64) @ t.1033 -> {"State#" %s.63} @ t.1034 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1035} @ t.1036
    "readWord8ArrayAsWord16#"     :: {"MutableByteArray#" %s.64} @ t.1037 -> (T_Int64) @ t.1038 -> {"State#" %s.64} @ t.1039 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1040} @ t.1041
    "readWord8ArrayAsWord32#"     :: {"MutableByteArray#" %s.65} @ t.1042 -> (T_Int64) @ t.1043 -> {"State#" %s.65} @ t.1044 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1045} @ t.1046
    "readWord8ArrayAsWord64#"     :: {"MutableByteArray#" %s.66} @ t.1047 -> (T_Int64) @ t.1048 -> {"State#" %s.66} @ t.1049 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1050} @ t.1051
    "readWord8ArrayAsWord#"       :: {"MutableByteArray#" %s.67} @ t.1052 -> (T_Int64) @ t.1053 -> {"State#" %s.67} @ t.1054 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1055} @ t.1056
    "writeCharArray#"             :: {"MutableByteArray#" %s.68} @ t.1057 -> (T_Int64) @ t.1058 -> (T_Char) @ t.1059 -> {"State#" %s.68} @ t.1060 -> {"GHC.Prim.(##)"} @ t.1061
    "writeWideCharArray#"         :: {"MutableByteArray#" %s.69} @ t.1062 -> (T_Int64) @ t.1063 -> (T_Char) @ t.1064 -> {"State#" %s.69} @ t.1065 -> {"GHC.Prim.(##)"} @ t.1066
    "writeIntArray#"              :: {"MutableByteArray#" %s.70} @ t.1067 -> (T_Int64) @ t.1068 -> (T_Int64) @ t.1069 -> {"State#" %s.70} @ t.1070 -> {"GHC.Prim.(##)"} @ t.1071
    "writeWordArray#"             :: {"MutableByteArray#" %s.71} @ t.1072 -> (T_Int64) @ t.1073 -> (T_Word64) @ t.1074 -> {"State#" %s.71} @ t.1075 -> {"GHC.Prim.(##)"} @ t.1076
    "writeAddrArray#"             :: {"MutableByteArray#" %s.72} @ t.1077 -> (T_Int64) @ t.1078 -> (T_Addr) @ t.1079 -> {"State#" %s.72} @ t.1080 -> {"GHC.Prim.(##)"} @ t.1081
    "writeFloatArray#"            :: {"MutableByteArray#" %s.73} @ t.1082 -> (T_Int64) @ t.1083 -> (T_Float) @ t.1084 -> {"State#" %s.73} @ t.1085 -> {"GHC.Prim.(##)"} @ t.1086
    "writeDoubleArray#"           :: {"MutableByteArray#" %s.74} @ t.1087 -> (T_Int64) @ t.1088 -> (T_Double) @ t.1089 -> {"State#" %s.74} @ t.1090 -> {"GHC.Prim.(##)"} @ t.1091
    "writeStablePtrArray#"        :: {"MutableByteArray#" %s.75} @ t.1092 -> (T_Int64) @ t.1093 -> {"StablePtr#" %a.38} @ t.1094 -> {"State#" %s.75} @ t.1095 -> {"GHC.Prim.(##)"} @ t.1096
    "writeInt8Array#"             :: {"MutableByteArray#" %s.76} @ t.1097 -> (T_Int64) @ t.1098 -> (T_Int64) @ t.1099 -> {"State#" %s.76} @ t.1100 -> {"GHC.Prim.(##)"} @ t.1101
    "writeInt16Array#"            :: {"MutableByteArray#" %s.77} @ t.1102 -> (T_Int64) @ t.1103 -> (T_Int64) @ t.1104 -> {"State#" %s.77} @ t.1105 -> {"GHC.Prim.(##)"} @ t.1106
    "writeInt32Array#"            :: {"MutableByteArray#" %s.78} @ t.1107 -> (T_Int64) @ t.1108 -> (T_Int64) @ t.1109 -> {"State#" %s.78} @ t.1110 -> {"GHC.Prim.(##)"} @ t.1111
    "writeInt64Array#"            :: {"MutableByteArray#" %s.79} @ t.1112 -> (T_Int64) @ t.1113 -> (T_Int64) @ t.1114 -> {"State#" %s.79} @ t.1115 -> {"GHC.Prim.(##)"} @ t.1116
    "writeWord8Array#"            :: {"MutableByteArray#" %s.80} @ t.1117 -> (T_Int64) @ t.1118 -> (T_Word64) @ t.1119 -> {"State#" %s.80} @ t.1120 -> {"GHC.Prim.(##)"} @ t.1121
    "writeWord16Array#"           :: {"MutableByteArray#" %s.81} @ t.1122 -> (T_Int64) @ t.1123 -> (T_Word64) @ t.1124 -> {"State#" %s.81} @ t.1125 -> {"GHC.Prim.(##)"} @ t.1126
    "writeWord32Array#"           :: {"MutableByteArray#" %s.82} @ t.1127 -> (T_Int64) @ t.1128 -> (T_Word64) @ t.1129 -> {"State#" %s.82} @ t.1130 -> {"GHC.Prim.(##)"} @ t.1131
    "writeWord64Array#"           :: {"MutableByteArray#" %s.83} @ t.1132 -> (T_Int64) @ t.1133 -> (T_Word64) @ t.1134 -> {"State#" %s.83} @ t.1135 -> {"GHC.Prim.(##)"} @ t.1136
    "writeWord8ArrayAsChar#"      :: {"MutableByteArray#" %s.84} @ t.1137 -> (T_Int64) @ t.1138 -> (T_Char) @ t.1139 -> {"State#" %s.84} @ t.1140 -> {"GHC.Prim.(##)"} @ t.1141
    "writeWord8ArrayAsWideChar#"  :: {"MutableByteArray#" %s.85} @ t.1142 -> (T_Int64) @ t.1143 -> (T_Char) @ t.1144 -> {"State#" %s.85} @ t.1145 -> {"GHC.Prim.(##)"} @ t.1146
    "writeWord8ArrayAsAddr#"      :: {"MutableByteArray#" %s.86} @ t.1147 -> (T_Int64) @ t.1148 -> (T_Addr) @ t.1149 -> {"State#" %s.86} @ t.1150 -> {"GHC.Prim.(##)"} @ t.1151
    "writeWord8ArrayAsFloat#"     :: {"MutableByteArray#" %s.87} @ t.1152 -> (T_Int64) @ t.1153 -> (T_Float) @ t.1154 -> {"State#" %s.87} @ t.1155 -> {"GHC.Prim.(##)"} @ t.1156
    "writeWord8ArrayAsDouble#"    :: {"MutableByteArray#" %s.88} @ t.1157 -> (T_Int64) @ t.1158 -> (T_Double) @ t.1159 -> {"State#" %s.88} @ t.1160 -> {"GHC.Prim.(##)"} @ t.1161
    "writeWord8ArrayAsStablePtr#" :: {"MutableByteArray#" %s.89} @ t.1162 -> (T_Int64) @ t.1163 -> {"StablePtr#" %a.39} @ t.1164 -> {"State#" %s.89} @ t.1165 -> {"GHC.Prim.(##)"} @ t.1166
    "writeWord8ArrayAsInt16#"     :: {"MutableByteArray#" %s.90} @ t.1167 -> (T_Int64) @ t.1168 -> (T_Int64) @ t.1169 -> {"State#" %s.90} @ t.1170 -> {"GHC.Prim.(##)"} @ t.1171
    "writeWord8ArrayAsInt32#"     :: {"MutableByteArray#" %s.91} @ t.1172 -> (T_Int64) @ t.1173 -> (T_Int64) @ t.1174 -> {"State#" %s.91} @ t.1175 -> {"GHC.Prim.(##)"} @ t.1176
    "writeWord8ArrayAsInt64#"     :: {"MutableByteArray#" %s.92} @ t.1177 -> (T_Int64) @ t.1178 -> (T_Int64) @ t.1179 -> {"State#" %s.92} @ t.1180 -> {"GHC.Prim.(##)"} @ t.1181
    "writeWord8ArrayAsInt#"       :: {"MutableByteArray#" %s.93} @ t.1182 -> (T_Int64) @ t.1183 -> (T_Int64) @ t.1184 -> {"State#" %s.93} @ t.1185 -> {"GHC.Prim.(##)"} @ t.1186
    "writeWord8ArrayAsWord16#"    :: {"MutableByteArray#" %s.94} @ t.1187 -> (T_Int64) @ t.1188 -> (T_Word64) @ t.1189 -> {"State#" %s.94} @ t.1190 -> {"GHC.Prim.(##)"} @ t.1191
    "writeWord8ArrayAsWord32#"    :: {"MutableByteArray#" %s.95} @ t.1192 -> (T_Int64) @ t.1193 -> (T_Word64) @ t.1194 -> {"State#" %s.95} @ t.1195 -> {"GHC.Prim.(##)"} @ t.1196
    "writeWord8ArrayAsWord64#"    :: {"MutableByteArray#" %s.96} @ t.1197 -> (T_Int64) @ t.1198 -> (T_Word64) @ t.1199 -> {"State#" %s.96} @ t.1200 -> {"GHC.Prim.(##)"} @ t.1201
    "writeWord8ArrayAsWord#"      :: {"MutableByteArray#" %s.97} @ t.1202 -> (T_Int64) @ t.1203 -> (T_Word64) @ t.1204 -> {"State#" %s.97} @ t.1205 -> {"GHC.Prim.(##)"} @ t.1206

  primop pure
    "compareByteArrays#" :: {"ByteArray#"} @ t.1207 -> (T_Int64) @ t.1208 -> {"ByteArray#"} @ t.1209 -> (T_Int64) @ t.1210 -> (T_Int64) @ t.1211 -> (T_Int64) @ t.1212

  primop effectful
    "copyByteArray#"              :: {"ByteArray#"} @ t.1213 -> (T_Int64) @ t.1214 -> {"MutableByteArray#" %s.98} @ t.1215 -> (T_Int64) @ t.1216 -> (T_Int64) @ t.1217 -> {"State#" %s.98} @ t.1218 -> {"GHC.Prim.(##)"} @ t.1219
    "copyMutableByteArray#"       :: {"MutableByteArray#" %s.99} @ t.1220 -> (T_Int64) @ t.1221 -> {"MutableByteArray#" %s.99} @ t.1222 -> (T_Int64) @ t.1223 -> (T_Int64) @ t.1224 -> {"State#" %s.99} @ t.1225 -> {"GHC.Prim.(##)"} @ t.1226
    "copyByteArrayToAddr#"        :: {"ByteArray#"} @ t.1227 -> (T_Int64) @ t.1228 -> (T_Addr) @ t.1229 -> (T_Int64) @ t.1230 -> {"State#" %s.100} @ t.1231 -> {"GHC.Prim.(##)"} @ t.1232
    "copyMutableByteArrayToAddr#" :: {"MutableByteArray#" %s.101} @ t.1233 -> (T_Int64) @ t.1234 -> (T_Addr) @ t.1235 -> (T_Int64) @ t.1236 -> {"State#" %s.101} @ t.1237 -> {"GHC.Prim.(##)"} @ t.1238
    "copyAddrToByteArray#"        :: (T_Addr) @ t.1239 -> {"MutableByteArray#" %s.102} @ t.1240 -> (T_Int64) @ t.1241 -> (T_Int64) @ t.1242 -> {"State#" %s.102} @ t.1243 -> {"GHC.Prim.(##)"} @ t.1244
    "setByteArray#"               :: {"MutableByteArray#" %s.103} @ t.1245 -> (T_Int64) @ t.1246 -> (T_Int64) @ t.1247 -> (T_Int64) @ t.1248 -> {"State#" %s.103} @ t.1249 -> {"GHC.Prim.(##)"} @ t.1250
    "atomicReadIntArray#"         :: {"MutableByteArray#" %s.104} @ t.1251 -> (T_Int64) @ t.1252 -> {"State#" %s.104} @ t.1253 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1254} @ t.1255
    "atomicWriteIntArray#"        :: {"MutableByteArray#" %s.105} @ t.1256 -> (T_Int64) @ t.1257 -> (T_Int64) @ t.1258 -> {"State#" %s.105} @ t.1259 -> {"GHC.Prim.(##)"} @ t.1260
    "casIntArray#"                :: {"MutableByteArray#" %s.106} @ t.1261 -> (T_Int64) @ t.1262 -> (T_Int64) @ t.1263 -> (T_Int64) @ t.1264 -> {"State#" %s.106} @ t.1265 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1266} @ t.1267
    "fetchAddIntArray#"           :: {"MutableByteArray#" %s.107} @ t.1268 -> (T_Int64) @ t.1269 -> (T_Int64) @ t.1270 -> {"State#" %s.107} @ t.1271 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1272} @ t.1273
    "fetchSubIntArray#"           :: {"MutableByteArray#" %s.108} @ t.1274 -> (T_Int64) @ t.1275 -> (T_Int64) @ t.1276 -> {"State#" %s.108} @ t.1277 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1278} @ t.1279
    "fetchAndIntArray#"           :: {"MutableByteArray#" %s.109} @ t.1280 -> (T_Int64) @ t.1281 -> (T_Int64) @ t.1282 -> {"State#" %s.109} @ t.1283 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1284} @ t.1285
    "fetchNandIntArray#"          :: {"MutableByteArray#" %s.110} @ t.1286 -> (T_Int64) @ t.1287 -> (T_Int64) @ t.1288 -> {"State#" %s.110} @ t.1289 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1290} @ t.1291
    "fetchOrIntArray#"            :: {"MutableByteArray#" %s.111} @ t.1292 -> (T_Int64) @ t.1293 -> (T_Int64) @ t.1294 -> {"State#" %s.111} @ t.1295 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1296} @ t.1297
    "fetchXorIntArray#"           :: {"MutableByteArray#" %s.112} @ t.1298 -> (T_Int64) @ t.1299 -> (T_Int64) @ t.1300 -> {"State#" %s.112} @ t.1301 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1302} @ t.1303

  {-
    Arrays of arrays
  -}
  primop effectful
    "newArrayArray#" :: (T_Int64) @ t.1304 -> {"State#" %s.113} @ t.1305 -> {"GHC.Prim.Unit#" {"MutableArrayArray#" %s.113} @ t.1306} @ t.1307

  primop pure
    "sameMutableArrayArray#" :: {"MutableArrayArray#" %s.114} @ t.1308 -> {"MutableArrayArray#" %s.114} @ t.1309 -> (T_Int64) @ t.1310

  primop effectful
    "unsafeFreezeArrayArray#" :: {"MutableArrayArray#" %s.115} @ t.1311 -> {"State#" %s.115} @ t.1312 -> {"GHC.Prim.Unit#" {"ArrayArray#"} @ t.1313} @ t.1314

  primop pure
    "sizeofArrayArray#"        :: {"ArrayArray#"} @ t.1315 -> (T_Int64) @ t.1316
    "sizeofMutableArrayArray#" :: {"MutableArrayArray#" %s.116} @ t.1317 -> (T_Int64) @ t.1318
    "indexByteArrayArray#"     :: {"ArrayArray#"} @ t.1319 -> (T_Int64) @ t.1320 -> {"ByteArray#"} @ t.1321
    "indexArrayArrayArray#"    :: {"ArrayArray#"} @ t.1322 -> (T_Int64) @ t.1323 -> {"ArrayArray#"} @ t.1324

  primop effectful
    "readByteArrayArray#"          :: {"MutableArrayArray#" %s.117} @ t.1325 -> (T_Int64) @ t.1326 -> {"State#" %s.117} @ t.1327 -> {"GHC.Prim.Unit#" {"ByteArray#"} @ t.1328} @ t.1329
    "readMutableByteArrayArray#"   :: {"MutableArrayArray#" %s.118} @ t.1330 -> (T_Int64) @ t.1331 -> {"State#" %s.118} @ t.1332 -> {"GHC.Prim.Unit#" {"MutableByteArray#" %s.118} @ t.1333} @ t.1334
    "readArrayArrayArray#"         :: {"MutableArrayArray#" %s.119} @ t.1335 -> (T_Int64) @ t.1336 -> {"State#" %s.119} @ t.1337 -> {"GHC.Prim.Unit#" {"ArrayArray#"} @ t.1338} @ t.1339
    "readMutableArrayArrayArray#"  :: {"MutableArrayArray#" %s.120} @ t.1340 -> (T_Int64) @ t.1341 -> {"State#" %s.120} @ t.1342 -> {"GHC.Prim.Unit#" {"MutableArrayArray#" %s.120} @ t.1343} @ t.1344
    "writeByteArrayArray#"         :: {"MutableArrayArray#" %s.121} @ t.1345 -> (T_Int64) @ t.1346 -> {"ByteArray#"} @ t.1347 -> {"State#" %s.121} @ t.1348 -> {"GHC.Prim.(##)"} @ t.1349
    "writeMutableByteArrayArray#"  :: {"MutableArrayArray#" %s.122} @ t.1350 -> (T_Int64) @ t.1351 -> {"MutableByteArray#" %s.122} @ t.1352 -> {"State#" %s.122} @ t.1353 -> {"GHC.Prim.(##)"} @ t.1354
    "writeArrayArrayArray#"        :: {"MutableArrayArray#" %s.123} @ t.1355 -> (T_Int64) @ t.1356 -> {"ArrayArray#"} @ t.1357 -> {"State#" %s.123} @ t.1358 -> {"GHC.Prim.(##)"} @ t.1359
    "writeMutableArrayArrayArray#" :: {"MutableArrayArray#" %s.124} @ t.1360 -> (T_Int64) @ t.1361 -> {"MutableArrayArray#" %s.124} @ t.1362 -> {"State#" %s.124} @ t.1363 -> {"GHC.Prim.(##)"} @ t.1364
    "copyArrayArray#"              :: {"ArrayArray#"} @ t.1365 -> (T_Int64) @ t.1366 -> {"MutableArrayArray#" %s.125} @ t.1367 -> (T_Int64) @ t.1368 -> (T_Int64) @ t.1369 -> {"State#" %s.125} @ t.1370 -> {"GHC.Prim.(##)"} @ t.1371
    "copyMutableArrayArray#"       :: {"MutableArrayArray#" %s.126} @ t.1372 -> (T_Int64) @ t.1373 -> {"MutableArrayArray#" %s.126} @ t.1374 -> (T_Int64) @ t.1375 -> (T_Int64) @ t.1376 -> {"State#" %s.126} @ t.1377 -> {"GHC.Prim.(##)"} @ t.1378

  {-
    Addr#
  -}
  primop pure
    "plusAddr#"              :: (T_Addr) @ t.1379 -> (T_Int64) @ t.1380 -> (T_Addr) @ t.1381
    "minusAddr#"             :: (T_Addr) @ t.1382 -> (T_Addr) @ t.1383 -> (T_Int64) @ t.1384
    "remAddr#"               :: (T_Addr) @ t.1385 -> (T_Int64) @ t.1386 -> (T_Int64) @ t.1387
    "addr2Int#"              :: (T_Addr) @ t.1388 -> (T_Int64) @ t.1389
    "int2Addr#"              :: (T_Int64) @ t.1390 -> (T_Addr) @ t.1391
    "gtAddr#"                :: (T_Addr) @ t.1392 -> (T_Addr) @ t.1393 -> (T_Int64) @ t.1394
    "geAddr#"                :: (T_Addr) @ t.1395 -> (T_Addr) @ t.1396 -> (T_Int64) @ t.1397
    "eqAddr#"                :: (T_Addr) @ t.1398 -> (T_Addr) @ t.1399 -> (T_Int64) @ t.1400
    "neAddr#"                :: (T_Addr) @ t.1401 -> (T_Addr) @ t.1402 -> (T_Int64) @ t.1403
    "ltAddr#"                :: (T_Addr) @ t.1404 -> (T_Addr) @ t.1405 -> (T_Int64) @ t.1406
    "leAddr#"                :: (T_Addr) @ t.1407 -> (T_Addr) @ t.1408 -> (T_Int64) @ t.1409
    "indexCharOffAddr#"      :: (T_Addr) @ t.1410 -> (T_Int64) @ t.1411 -> (T_Char) @ t.1412
    "indexWideCharOffAddr#"  :: (T_Addr) @ t.1413 -> (T_Int64) @ t.1414 -> (T_Char) @ t.1415
    "indexIntOffAddr#"       :: (T_Addr) @ t.1416 -> (T_Int64) @ t.1417 -> (T_Int64) @ t.1418
    "indexWordOffAddr#"      :: (T_Addr) @ t.1419 -> (T_Int64) @ t.1420 -> (T_Word64) @ t.1421
    "indexAddrOffAddr#"      :: (T_Addr) @ t.1422 -> (T_Int64) @ t.1423 -> (T_Addr) @ t.1424
    "indexFloatOffAddr#"     :: (T_Addr) @ t.1425 -> (T_Int64) @ t.1426 -> (T_Float) @ t.1427
    "indexDoubleOffAddr#"    :: (T_Addr) @ t.1428 -> (T_Int64) @ t.1429 -> (T_Double) @ t.1430
    "indexStablePtrOffAddr#" :: (T_Addr) @ t.1431 -> (T_Int64) @ t.1432 -> {"StablePtr#" %a.40} @ t.1433
    "indexInt8OffAddr#"      :: (T_Addr) @ t.1434 -> (T_Int64) @ t.1435 -> (T_Int64) @ t.1436
    "indexInt16OffAddr#"     :: (T_Addr) @ t.1437 -> (T_Int64) @ t.1438 -> (T_Int64) @ t.1439
    "indexInt32OffAddr#"     :: (T_Addr) @ t.1440 -> (T_Int64) @ t.1441 -> (T_Int64) @ t.1442
    "indexInt64OffAddr#"     :: (T_Addr) @ t.1443 -> (T_Int64) @ t.1444 -> (T_Int64) @ t.1445
    "indexWord8OffAddr#"     :: (T_Addr) @ t.1446 -> (T_Int64) @ t.1447 -> (T_Word64) @ t.1448
    "indexWord16OffAddr#"    :: (T_Addr) @ t.1449 -> (T_Int64) @ t.1450 -> (T_Word64) @ t.1451
    "indexWord32OffAddr#"    :: (T_Addr) @ t.1452 -> (T_Int64) @ t.1453 -> (T_Word64) @ t.1454
    "indexWord64OffAddr#"    :: (T_Addr) @ t.1455 -> (T_Int64) @ t.1456 -> (T_Word64) @ t.1457

  primop effectful
    "readCharOffAddr#"       :: (T_Addr) @ t.1458 -> (T_Int64) @ t.1459 -> {"State#" %s.127} @ t.1460 -> {"GHC.Prim.Unit#" (T_Char) @ t.1461} @ t.1462
    "readWideCharOffAddr#"   :: (T_Addr) @ t.1463 -> (T_Int64) @ t.1464 -> {"State#" %s.128} @ t.1465 -> {"GHC.Prim.Unit#" (T_Char) @ t.1466} @ t.1467
    "readIntOffAddr#"        :: (T_Addr) @ t.1468 -> (T_Int64) @ t.1469 -> {"State#" %s.129} @ t.1470 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1471} @ t.1472
    "readWordOffAddr#"       :: (T_Addr) @ t.1473 -> (T_Int64) @ t.1474 -> {"State#" %s.130} @ t.1475 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1476} @ t.1477
    "readAddrOffAddr#"       :: (T_Addr) @ t.1478 -> (T_Int64) @ t.1479 -> {"State#" %s.131} @ t.1480 -> {"GHC.Prim.Unit#" (T_Addr) @ t.1481} @ t.1482
    "readFloatOffAddr#"      :: (T_Addr) @ t.1483 -> (T_Int64) @ t.1484 -> {"State#" %s.132} @ t.1485 -> {"GHC.Prim.Unit#" (T_Float) @ t.1486} @ t.1487
    "readDoubleOffAddr#"     :: (T_Addr) @ t.1488 -> (T_Int64) @ t.1489 -> {"State#" %s.133} @ t.1490 -> {"GHC.Prim.Unit#" (T_Double) @ t.1491} @ t.1492
    "readStablePtrOffAddr#"  :: (T_Addr) @ t.1493 -> (T_Int64) @ t.1494 -> {"State#" %s.134} @ t.1495 -> {"GHC.Prim.Unit#" {"StablePtr#" %a.41} @ t.1496} @ t.1497
    "readInt8OffAddr#"       :: (T_Addr) @ t.1498 -> (T_Int64) @ t.1499 -> {"State#" %s.135} @ t.1500 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1501} @ t.1502
    "readInt16OffAddr#"      :: (T_Addr) @ t.1503 -> (T_Int64) @ t.1504 -> {"State#" %s.136} @ t.1505 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1506} @ t.1507
    "readInt32OffAddr#"      :: (T_Addr) @ t.1508 -> (T_Int64) @ t.1509 -> {"State#" %s.137} @ t.1510 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1511} @ t.1512
    "readInt64OffAddr#"      :: (T_Addr) @ t.1513 -> (T_Int64) @ t.1514 -> {"State#" %s.138} @ t.1515 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1516} @ t.1517
    "readWord8OffAddr#"      :: (T_Addr) @ t.1518 -> (T_Int64) @ t.1519 -> {"State#" %s.139} @ t.1520 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1521} @ t.1522
    "readWord16OffAddr#"     :: (T_Addr) @ t.1523 -> (T_Int64) @ t.1524 -> {"State#" %s.140} @ t.1525 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1526} @ t.1527
    "readWord32OffAddr#"     :: (T_Addr) @ t.1528 -> (T_Int64) @ t.1529 -> {"State#" %s.141} @ t.1530 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1531} @ t.1532
    "readWord64OffAddr#"     :: (T_Addr) @ t.1533 -> (T_Int64) @ t.1534 -> {"State#" %s.142} @ t.1535 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1536} @ t.1537
    "writeCharOffAddr#"      :: (T_Addr) @ t.1538 -> (T_Int64) @ t.1539 -> (T_Char) @ t.1540 -> {"State#" %s.143} @ t.1541 -> {"GHC.Prim.(##)"} @ t.1542
    "writeWideCharOffAddr#"  :: (T_Addr) @ t.1543 -> (T_Int64) @ t.1544 -> (T_Char) @ t.1545 -> {"State#" %s.144} @ t.1546 -> {"GHC.Prim.(##)"} @ t.1547
    "writeIntOffAddr#"       :: (T_Addr) @ t.1548 -> (T_Int64) @ t.1549 -> (T_Int64) @ t.1550 -> {"State#" %s.145} @ t.1551 -> {"GHC.Prim.(##)"} @ t.1552
    "writeWordOffAddr#"      :: (T_Addr) @ t.1553 -> (T_Int64) @ t.1554 -> (T_Word64) @ t.1555 -> {"State#" %s.146} @ t.1556 -> {"GHC.Prim.(##)"} @ t.1557
    "writeAddrOffAddr#"      :: (T_Addr) @ t.1558 -> (T_Int64) @ t.1559 -> (T_Addr) @ t.1560 -> {"State#" %s.147} @ t.1561 -> {"GHC.Prim.(##)"} @ t.1562
    "writeFloatOffAddr#"     :: (T_Addr) @ t.1563 -> (T_Int64) @ t.1564 -> (T_Float) @ t.1565 -> {"State#" %s.148} @ t.1566 -> {"GHC.Prim.(##)"} @ t.1567
    "writeDoubleOffAddr#"    :: (T_Addr) @ t.1568 -> (T_Int64) @ t.1569 -> (T_Double) @ t.1570 -> {"State#" %s.149} @ t.1571 -> {"GHC.Prim.(##)"} @ t.1572
    "writeStablePtrOffAddr#" :: (T_Addr) @ t.1573 -> (T_Int64) @ t.1574 -> {"StablePtr#" %a.42} @ t.1575 -> {"State#" %s.150} @ t.1576 -> {"GHC.Prim.(##)"} @ t.1577
    "writeInt8OffAddr#"      :: (T_Addr) @ t.1578 -> (T_Int64) @ t.1579 -> (T_Int64) @ t.1580 -> {"State#" %s.151} @ t.1581 -> {"GHC.Prim.(##)"} @ t.1582
    "writeInt16OffAddr#"     :: (T_Addr) @ t.1583 -> (T_Int64) @ t.1584 -> (T_Int64) @ t.1585 -> {"State#" %s.152} @ t.1586 -> {"GHC.Prim.(##)"} @ t.1587
    "writeInt32OffAddr#"     :: (T_Addr) @ t.1588 -> (T_Int64) @ t.1589 -> (T_Int64) @ t.1590 -> {"State#" %s.153} @ t.1591 -> {"GHC.Prim.(##)"} @ t.1592
    "writeInt64OffAddr#"     :: (T_Addr) @ t.1593 -> (T_Int64) @ t.1594 -> (T_Int64) @ t.1595 -> {"State#" %s.154} @ t.1596 -> {"GHC.Prim.(##)"} @ t.1597
    "writeWord8OffAddr#"     :: (T_Addr) @ t.1598 -> (T_Int64) @ t.1599 -> (T_Word64) @ t.1600 -> {"State#" %s.155} @ t.1601 -> {"GHC.Prim.(##)"} @ t.1602
    "writeWord16OffAddr#"    :: (T_Addr) @ t.1603 -> (T_Int64) @ t.1604 -> (T_Word64) @ t.1605 -> {"State#" %s.156} @ t.1606 -> {"GHC.Prim.(##)"} @ t.1607
    "writeWord32OffAddr#"    :: (T_Addr) @ t.1608 -> (T_Int64) @ t.1609 -> (T_Word64) @ t.1610 -> {"State#" %s.157} @ t.1611 -> {"GHC.Prim.(##)"} @ t.1612
    "writeWord64OffAddr#"    :: (T_Addr) @ t.1613 -> (T_Int64) @ t.1614 -> (T_Word64) @ t.1615 -> {"State#" %s.158} @ t.1616 -> {"GHC.Prim.(##)"} @ t.1617

  {-
    Mutable variables
  -}
  primop effectful
    "newMutVar#"   :: %a.43 -> {"State#" %s.159} @ t.1618 -> {"GHC.Prim.Unit#" {"MutVar#" %s.159 %a.43} @ t.1619} @ t.1620
    "readMutVar#"  :: {"MutVar#" %s.160 %a.44} @ t.1621 -> {"State#" %s.160} @ t.1622 -> {"GHC.Prim.Unit#" %a.44} @ t.1623
    "writeMutVar#" :: {"MutVar#" %s.161 %a.45} @ t.1624 -> %a.45 -> {"State#" %s.161} @ t.1625 -> {"GHC.Prim.(##)"} @ t.1626

  primop pure
    "sameMutVar#" :: {"MutVar#" %s.162 %a.46} @ t.1627 -> {"MutVar#" %s.162 %a.46} @ t.1628 -> (T_Int64) @ t.1629

  primop effectful
    "atomicModifyMutVar2#" :: {"MutVar#" %s.163 %a.47} @ t.1630 -> (tf.1060 : %a.47 -> %c.0) -> {"State#" %s.163} @ t.1631 -> {"GHC.Prim.(#,#)" %a.47 %c.0} @ t.1632
    "atomicModifyMutVar_#" :: {"MutVar#" %s.164 %a.48} @ t.1633 -> (tf.1064 : %a.48 -> %a.48) -> {"State#" %s.164} @ t.1634 -> {"GHC.Prim.(#,#)" %a.48 %a.48} @ t.1635
    "casMutVar#"           :: {"MutVar#" %s.165 %a.49} @ t.1636 -> %a.49 -> %a.49 -> {"State#" %s.165} @ t.1637 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.1638 %a.49} @ t.1639

  {-
    Exceptions
  -}
  primop effectful
    "catch#"                 :: (tf.1072 : {"State#" {RealWorld} @ t.1640} @ t.1641 -> {"GHC.Prim.Unit#" %a.50} @ t.1642) -> (tf.1074 : %b.0 -> {"State#" {RealWorld} @ t.1643} @ t.1644 -> {"GHC.Prim.Unit#" %a.50} @ t.1645) -> {"State#" {RealWorld} @ t.1646} @ t.1647 -> {"GHC.Prim.Unit#" %a.50} @ t.1648
    "raise#"                 :: %b.1 -> %o.0
    "raiseDivZero#"          :: {"Void#"} @ t.1649 -> %o.1
    "raiseUnderflow#"        :: {"Void#"} @ t.1650 -> %o.2
    "raiseOverflow#"         :: {"Void#"} @ t.1651 -> %o.3
    "raiseIO#"               :: %a.51 -> {"State#" {RealWorld} @ t.1652} @ t.1653 -> {"GHC.Prim.Unit#" %b.2} @ t.1654
    "maskAsyncExceptions#"   :: (tf.1084 : {"State#" {RealWorld} @ t.1655} @ t.1656 -> {"GHC.Prim.Unit#" %a.52} @ t.1657) -> {"State#" {RealWorld} @ t.1658} @ t.1659 -> {"GHC.Prim.Unit#" %a.52} @ t.1660
    "maskUninterruptible#"   :: (tf.1087 : {"State#" {RealWorld} @ t.1661} @ t.1662 -> {"GHC.Prim.Unit#" %a.53} @ t.1663) -> {"State#" {RealWorld} @ t.1664} @ t.1665 -> {"GHC.Prim.Unit#" %a.53} @ t.1666
    "unmaskAsyncExceptions#" :: (tf.1090 : {"State#" {RealWorld} @ t.1667} @ t.1668 -> {"GHC.Prim.Unit#" %a.54} @ t.1669) -> {"State#" {RealWorld} @ t.1670} @ t.1671 -> {"GHC.Prim.Unit#" %a.54} @ t.1672
    "getMaskingState#"       :: {"State#" {RealWorld} @ t.1673} @ t.1674 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1675} @ t.1676

  {-
    STM-accessible Mutable Variables
  -}
  primop effectful
    "atomically#" :: (tf.1094 : {"State#" {RealWorld} @ t.1677} @ t.1678 -> {"GHC.Prim.Unit#" %a.55} @ t.1679) -> {"State#" {RealWorld} @ t.1680} @ t.1681 -> {"GHC.Prim.Unit#" %a.55} @ t.1682
    "retry#"      :: {"State#" {RealWorld} @ t.1683} @ t.1684 -> {"GHC.Prim.Unit#" %a.56} @ t.1685
    "catchRetry#" :: (tf.1098 : {"State#" {RealWorld} @ t.1686} @ t.1687 -> {"GHC.Prim.Unit#" %a.57} @ t.1688) -> (tf.1099 : {"State#" {RealWorld} @ t.1689} @ t.1690 -> {"GHC.Prim.Unit#" %a.57} @ t.1691) -> {"State#" {RealWorld} @ t.1692} @ t.1693 -> {"GHC.Prim.Unit#" %a.57} @ t.1694
    "catchSTM#"   :: (tf.1103 : {"State#" {RealWorld} @ t.1695} @ t.1696 -> {"GHC.Prim.Unit#" %a.58} @ t.1697) -> (tf.1105 : %b.3 -> {"State#" {RealWorld} @ t.1698} @ t.1699 -> {"GHC.Prim.Unit#" %a.58} @ t.1700) -> {"State#" {RealWorld} @ t.1701} @ t.1702 -> {"GHC.Prim.Unit#" %a.58} @ t.1703
    "newTVar#"    :: %a.59 -> {"State#" %s.166} @ t.1704 -> {"GHC.Prim.Unit#" {"TVar#" %s.166 %a.59} @ t.1705} @ t.1706
    "readTVar#"   :: {"TVar#" %s.167 %a.60} @ t.1707 -> {"State#" %s.167} @ t.1708 -> {"GHC.Prim.Unit#" %a.60} @ t.1709
    "readTVarIO#" :: {"TVar#" %s.168 %a.61} @ t.1710 -> {"State#" %s.168} @ t.1711 -> {"GHC.Prim.Unit#" %a.61} @ t.1712
    "writeTVar#"  :: {"TVar#" %s.169 %a.62} @ t.1713 -> %a.62 -> {"State#" %s.169} @ t.1714 -> {"GHC.Prim.(##)"} @ t.1715

  primop pure
    "sameTVar#" :: {"TVar#" %s.170 %a.63} @ t.1716 -> {"TVar#" %s.170 %a.63} @ t.1717 -> (T_Int64) @ t.1718

  {-
    Synchronized Mutable Variables
  -}
  primop effectful
    "newMVar#"     :: {"State#" %s.171} @ t.1719 -> {"GHC.Prim.Unit#" {"MVar#" %s.171 %a.64} @ t.1720} @ t.1721
    "takeMVar#"    :: {"MVar#" %s.172 %a.65} @ t.1722 -> {"State#" %s.172} @ t.1723 -> {"GHC.Prim.Unit#" %a.65} @ t.1724
    "tryTakeMVar#" :: {"MVar#" %s.173 %a.66} @ t.1725 -> {"State#" %s.173} @ t.1726 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.1727 %a.66} @ t.1728
    "putMVar#"     :: {"MVar#" %s.174 %a.67} @ t.1729 -> %a.67 -> {"State#" %s.174} @ t.1730 -> {"GHC.Prim.(##)"} @ t.1731
    "tryPutMVar#"  :: {"MVar#" %s.175 %a.68} @ t.1732 -> %a.68 -> {"State#" %s.175} @ t.1733 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1734} @ t.1735
    "readMVar#"    :: {"MVar#" %s.176 %a.69} @ t.1736 -> {"State#" %s.176} @ t.1737 -> {"GHC.Prim.Unit#" %a.69} @ t.1738
    "tryReadMVar#" :: {"MVar#" %s.177 %a.70} @ t.1739 -> {"State#" %s.177} @ t.1740 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.1741 %a.70} @ t.1742

  primop pure
    "sameMVar#" :: {"MVar#" %s.178 %a.71} @ t.1743 -> {"MVar#" %s.178 %a.71} @ t.1744 -> (T_Int64) @ t.1745

  primop effectful
    "isEmptyMVar#" :: {"MVar#" %s.179 %a.72} @ t.1746 -> {"State#" %s.179} @ t.1747 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1748} @ t.1749

  {-
    Delay/wait operations
  -}
  primop effectful
    "delay#"     :: (T_Int64) @ t.1750 -> {"State#" %s.180} @ t.1751 -> {"GHC.Prim.(##)"} @ t.1752
    "waitRead#"  :: (T_Int64) @ t.1753 -> {"State#" %s.181} @ t.1754 -> {"GHC.Prim.(##)"} @ t.1755
    "waitWrite#" :: (T_Int64) @ t.1756 -> {"State#" %s.182} @ t.1757 -> {"GHC.Prim.(##)"} @ t.1758

  {-
    Concurrency primitives
  -}
  primop effectful
    "fork#"                 :: %a.73 -> {"State#" {RealWorld} @ t.1759} @ t.1760 -> {"GHC.Prim.Unit#" {"ThreadId#"} @ t.1761} @ t.1762
    "forkOn#"               :: (T_Int64) @ t.1763 -> %a.74 -> {"State#" {RealWorld} @ t.1764} @ t.1765 -> {"GHC.Prim.Unit#" {"ThreadId#"} @ t.1766} @ t.1767
    "killThread#"           :: {"ThreadId#"} @ t.1768 -> %a.75 -> {"State#" {RealWorld} @ t.1769} @ t.1770 -> {"GHC.Prim.(##)"} @ t.1771
    "yield#"                :: {"State#" {RealWorld} @ t.1772} @ t.1773 -> {"GHC.Prim.(##)"} @ t.1774
    "myThreadId#"           :: {"State#" {RealWorld} @ t.1775} @ t.1776 -> {"GHC.Prim.Unit#" {"ThreadId#"} @ t.1777} @ t.1778
    "labelThread#"          :: {"ThreadId#"} @ t.1779 -> (T_Addr) @ t.1780 -> {"State#" {RealWorld} @ t.1781} @ t.1782 -> {"GHC.Prim.(##)"} @ t.1783
    "isCurrentThreadBound#" :: {"State#" {RealWorld} @ t.1784} @ t.1785 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1786} @ t.1787
    "noDuplicate#"          :: {"State#" %s.183} @ t.1788 -> {"GHC.Prim.(##)"} @ t.1789
    "threadStatus#"         :: {"ThreadId#"} @ t.1790 -> {"State#" {RealWorld} @ t.1791} @ t.1792 -> {"GHC.Prim.(#,,#)" (T_Int64) @ t.1793 (T_Int64) @ t.1794 (T_Int64) @ t.1795} @ t.1796

  {-
    Weak pointers
  -}
  primop effectful
    "mkWeak#"              :: %o.4 -> %b.4 -> (tf.1162 : {"State#" {RealWorld} @ t.1797} @ t.1798 -> {"GHC.Prim.Unit#" %c.1} @ t.1799) -> {"State#" {RealWorld} @ t.1800} @ t.1801 -> {"GHC.Prim.Unit#" {"Weak#" %b.4} @ t.1802} @ t.1803
    "mkWeakNoFinalizer#"   :: %o.5 -> %b.5 -> {"State#" {RealWorld} @ t.1804} @ t.1805 -> {"GHC.Prim.Unit#" {"Weak#" %b.5} @ t.1806} @ t.1807
    "addCFinalizerToWeak#" :: (T_Addr) @ t.1808 -> (T_Addr) @ t.1809 -> (T_Int64) @ t.1810 -> (T_Addr) @ t.1811 -> {"Weak#" %b.6} @ t.1812 -> {"State#" {RealWorld} @ t.1813} @ t.1814 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1815} @ t.1816
    "deRefWeak#"           :: {"Weak#" %a.76} @ t.1817 -> {"State#" {RealWorld} @ t.1818} @ t.1819 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.1820 %a.76} @ t.1821
    "finalizeWeak#"        :: {"Weak#" %a.77} @ t.1822 -> {"State#" {RealWorld} @ t.1823} @ t.1824 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.1825 (tf.1178 : {"State#" {RealWorld} @ t.1826} @ t.1827 -> {"GHC.Prim.Unit#" %b.7} @ t.1828)} @ t.1829
    "touch#"               :: %o.6 -> {"State#" {RealWorld} @ t.1830} @ t.1831 -> {"GHC.Prim.(##)"} @ t.1832

  {-
    Stable pointers and names
  -}
  primop effectful
    "makeStablePtr#"  :: %a.78 -> {"State#" {RealWorld} @ t.1833} @ t.1834 -> {"GHC.Prim.Unit#" {"StablePtr#" %a.78} @ t.1835} @ t.1836
    "deRefStablePtr#" :: {"StablePtr#" %a.79} @ t.1837 -> {"State#" {RealWorld} @ t.1838} @ t.1839 -> {"GHC.Prim.Unit#" %a.79} @ t.1840
    "eqStablePtr#"    :: {"StablePtr#" %a.80} @ t.1841 -> {"StablePtr#" %a.80} @ t.1842 -> (T_Int64) @ t.1843
    "makeStableName#" :: %a.81 -> {"State#" {RealWorld} @ t.1844} @ t.1845 -> {"GHC.Prim.Unit#" {"StableName#" %a.81} @ t.1846} @ t.1847

  primop pure
    "eqStableName#"    :: {"StableName#" %a.82} @ t.1848 -> {"StableName#" %b.8} @ t.1849 -> (T_Int64) @ t.1850
    "stableNameToInt#" :: {"StableName#" %a.83} @ t.1851 -> (T_Int64) @ t.1852

  {-
    Compact normal form
  -}
  primop effectful
    "compactNew#"    :: (T_Word64) @ t.1853 -> {"State#" {RealWorld} @ t.1854} @ t.1855 -> {"GHC.Prim.Unit#" {"Compact#"} @ t.1856} @ t.1857
    "compactResize#" :: {"Compact#"} @ t.1858 -> (T_Word64) @ t.1859 -> {"State#" {RealWorld} @ t.1860} @ t.1861 -> {"GHC.Prim.(##)"} @ t.1862

  primop pure
    "compactContains#"      :: {"Compact#"} @ t.1863 -> %a.84 -> {"State#" {RealWorld} @ t.1864} @ t.1865 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1866} @ t.1867
    "compactContainsAny#"   :: %a.85 -> {"State#" {RealWorld} @ t.1868} @ t.1869 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1870} @ t.1871
    "compactGetFirstBlock#" :: {"Compact#"} @ t.1872 -> {"State#" {RealWorld} @ t.1873} @ t.1874 -> {"GHC.Prim.(#,#)" (T_Addr) @ t.1875 (T_Word64) @ t.1876} @ t.1877
    "compactGetNextBlock#"  :: {"Compact#"} @ t.1878 -> (T_Addr) @ t.1879 -> {"State#" {RealWorld} @ t.1880} @ t.1881 -> {"GHC.Prim.(#,#)" (T_Addr) @ t.1882 (T_Word64) @ t.1883} @ t.1884

  primop effectful
    "compactAllocateBlock#"  :: (T_Word64) @ t.1885 -> (T_Addr) @ t.1886 -> {"State#" {RealWorld} @ t.1887} @ t.1888 -> {"GHC.Prim.Unit#" (T_Addr) @ t.1889} @ t.1890
    "compactFixupPointers#"  :: (T_Addr) @ t.1891 -> (T_Addr) @ t.1892 -> {"State#" {RealWorld} @ t.1893} @ t.1894 -> {"GHC.Prim.(#,#)" {"Compact#"} @ t.1895 (T_Addr) @ t.1896} @ t.1897
    "compactAdd#"            :: {"Compact#"} @ t.1898 -> %a.86 -> {"State#" {RealWorld} @ t.1899} @ t.1900 -> {"GHC.Prim.Unit#" %a.86} @ t.1901
    "compactAddWithSharing#" :: {"Compact#"} @ t.1902 -> %a.87 -> {"State#" {RealWorld} @ t.1903} @ t.1904 -> {"GHC.Prim.Unit#" %a.87} @ t.1905
    "compactSize#"           :: {"Compact#"} @ t.1906 -> {"State#" {RealWorld} @ t.1907} @ t.1908 -> {"GHC.Prim.Unit#" (T_Word64) @ t.1909} @ t.1910

  {-
    Unsafe pointer equality
  -}
  primop pure
    "reallyUnsafePtrEquality#" :: %a.88 -> %a.88 -> (T_Int64) @ t.1911

  {-
    Parallelism
  -}
  primop effectful
    "par#"   :: %a.89 -> (T_Int64) @ t.1912
    "spark#" :: %a.90 -> {"State#" %s.184} @ t.1913 -> {"GHC.Prim.Unit#" %a.90} @ t.1914

  primop pure
    "seq#" :: %a.91 -> {"State#" %s.185} @ t.1915 -> {"GHC.Prim.Unit#" %a.91} @ t.1916

  primop effectful
    "getSpark#"  :: {"State#" %s.186} @ t.1917 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.1918 %a.92} @ t.1919
    "numSparks#" :: {"State#" %s.187} @ t.1920 -> {"GHC.Prim.Unit#" (T_Int64) @ t.1921} @ t.1922

  {-
    Tag to enum stuff
  -}
  primop pure
    "dataToTag#" :: %a.93 -> (T_Int64) @ t.1923
    "tagToEnum#" :: (T_Int64) @ t.1924 -> %a.94

  {-
    Bytecode operations
  -}
  primop pure
    "addrToAny#" :: (T_Addr) @ t.1925 -> {"GHC.Prim.Unit#" %a.95} @ t.1926
    "anyToAddr#" :: %a.96 -> {"State#" {RealWorld} @ t.1927} @ t.1928 -> {"GHC.Prim.Unit#" (T_Addr) @ t.1929} @ t.1930
    "mkApUpd0#"  :: {BCO} @ t.1931 -> {"GHC.Prim.Unit#" %a.97} @ t.1932

  primop effectful
    "newBCO#" :: {"ByteArray#"} @ t.1933 -> {"ByteArray#"} @ t.1934 -> {"Array#" %a.98} @ t.1935 -> (T_Int64) @ t.1936 -> {"ByteArray#"} @ t.1937 -> {"State#" %s.188} @ t.1938 -> {"GHC.Prim.Unit#" {BCO} @ t.1939} @ t.1940

  primop pure
    "unpackClosure#" :: %a.99 -> {"GHC.Prim.(#,,#)" (T_Addr) @ t.1941 {"ByteArray#"} @ t.1942 {"Array#" %b.9} @ t.1943} @ t.1944
    "closureSize#"   :: %a.100 -> (T_Int64) @ t.1945
    "getApStackVal#" :: %a.101 -> (T_Int64) @ t.1946 -> {"GHC.Prim.(#,#)" (T_Int64) @ t.1947 %b.10} @ t.1948

  {-
    Misc
  -}
  primop pure
    "getCCSOf#"      :: %a.102 -> {"State#" %s.189} @ t.1949 -> {"GHC.Prim.Unit#" (T_Addr) @ t.1950} @ t.1951
    "getCurrentCCS#" :: %a.103 -> {"State#" %s.190} @ t.1952 -> {"GHC.Prim.Unit#" (T_Addr) @ t.1953} @ t.1954
    "clearCCS#"      :: (tf.1252 : {"State#" %s.191} @ t.1955 -> {"GHC.Prim.Unit#" %a.104} @ t.1956) -> {"State#" %s.191} @ t.1957 -> {"GHC.Prim.Unit#" %a.104} @ t.1958

  {-
    Etc
  -}
  primop effectful
    "traceEvent#"                 :: (T_Addr) @ t.1959 -> {"State#" %s.192} @ t.1960 -> {"GHC.Prim.(##)"} @ t.1961
    "traceBinaryEvent#"           :: (T_Addr) @ t.1962 -> (T_Int64) @ t.1963 -> {"State#" %s.193} @ t.1964 -> {"GHC.Prim.(##)"} @ t.1965
    "traceMarker#"                :: (T_Addr) @ t.1966 -> {"State#" %s.194} @ t.1967 -> {"GHC.Prim.(##)"} @ t.1968
    "setThreadAllocationCounter#" :: (T_Int64) @ t.1969 -> {"State#" {RealWorld} @ t.1970} @ t.1971 -> {"GHC.Prim.(##)"} @ t.1972

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
    "prefetchByteArray3#"        :: {"ByteArray#"} @ t.2001 -> (T_Int64) @ t.2002 -> {"State#" %s.201} @ t.2003 -> {"GHC.Prim.(##)"} @ t.2004
    "prefetchMutableByteArray3#" :: {"MutableByteArray#" %s.202} @ t.2005 -> (T_Int64) @ t.2006 -> {"State#" %s.202} @ t.2007 -> {"GHC.Prim.(##)"} @ t.2008
    "prefetchAddr3#"             :: (T_Addr) @ t.2009 -> (T_Int64) @ t.2010 -> {"State#" %s.203} @ t.2011 -> {"GHC.Prim.(##)"} @ t.2012
    "prefetchValue3#"            :: %a.105 -> {"State#" %s.204} @ t.2013 -> {"GHC.Prim.(##)"} @ t.2014
    "prefetchByteArray2#"        :: {"ByteArray#"} @ t.2015 -> (T_Int64) @ t.2016 -> {"State#" %s.205} @ t.2017 -> {"GHC.Prim.(##)"} @ t.2018
    "prefetchMutableByteArray2#" :: {"MutableByteArray#" %s.206} @ t.2019 -> (T_Int64) @ t.2020 -> {"State#" %s.206} @ t.2021 -> {"GHC.Prim.(##)"} @ t.2022
    "prefetchAddr2#"             :: (T_Addr) @ t.2023 -> (T_Int64) @ t.2024 -> {"State#" %s.207} @ t.2025 -> {"GHC.Prim.(##)"} @ t.2026
    "prefetchValue2#"            :: %a.106 -> {"State#" %s.208} @ t.2027 -> {"GHC.Prim.(##)"} @ t.2028
    "prefetchByteArray1#"        :: {"ByteArray#"} @ t.2029 -> (T_Int64) @ t.2030 -> {"State#" %s.209} @ t.2031 -> {"GHC.Prim.(##)"} @ t.2032
    "prefetchMutableByteArray1#" :: {"MutableByteArray#" %s.210} @ t.2033 -> (T_Int64) @ t.2034 -> {"State#" %s.210} @ t.2035 -> {"GHC.Prim.(##)"} @ t.2036
    "prefetchAddr1#"             :: (T_Addr) @ t.2037 -> (T_Int64) @ t.2038 -> {"State#" %s.211} @ t.2039 -> {"GHC.Prim.(##)"} @ t.2040
    "prefetchValue1#"            :: %a.107 -> {"State#" %s.212} @ t.2041 -> {"GHC.Prim.(##)"} @ t.2042
    "prefetchByteArray0#"        :: {"ByteArray#"} @ t.2043 -> (T_Int64) @ t.2044 -> {"State#" %s.213} @ t.2045 -> {"GHC.Prim.(##)"} @ t.2046
    "prefetchMutableByteArray0#" :: {"MutableByteArray#" %s.214} @ t.2047 -> (T_Int64) @ t.2048 -> {"State#" %s.214} @ t.2049 -> {"GHC.Prim.(##)"} @ t.2050
    "prefetchAddr0#"             :: (T_Addr) @ t.2051 -> (T_Int64) @ t.2052 -> {"State#" %s.215} @ t.2053 -> {"GHC.Prim.(##)"} @ t.2054
    "prefetchValue0#"            :: %a.108 -> {"State#" %s.216} @ t.2055 -> {"GHC.Prim.(##)"} @ t.2056

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
  , "broadcast#"                             -- unsupported type: TyApp SCALAR []
  , "pack#"                                  -- unsupported type: TyApp VECTUPLE []
  , "unpack#"                                -- unsupported type: TyApp VECTOR []
  , "insert#"                                -- unsupported type: TyApp VECTOR []
  , "plus#"                                  -- unsupported type: TyApp VECTOR []
  , "minus#"                                 -- unsupported type: TyApp VECTOR []
  , "times#"                                 -- unsupported type: TyApp VECTOR []
  , "divide#"                                -- unsupported type: TyApp VECTOR []
  , "quot#"                                  -- unsupported type: TyApp VECTOR []
  , "rem#"                                   -- unsupported type: TyApp VECTOR []
  , "negate#"                                -- unsupported type: TyApp VECTOR []
  , "indexArray#"                            -- unsupported type: TyApp VECTOR []
  , "readArray#"                             -- unsupported type: TyApp VECTOR []
  , "writeArray#"                            -- unsupported type: TyApp VECTOR []
  , "indexOffAddr#"                          -- unsupported type: TyApp VECTOR []
  , "readOffAddr#"                           -- unsupported type: TyApp VECTOR []
  , "writeOffAddr#"                          -- unsupported type: TyApp VECTOR []
  , "indexArrayAs#"                          -- unsupported type: TyApp VECTOR []
  , "readArrayAs#"                           -- unsupported type: TyApp VECTOR []
  , "writeArrayAs#"                          -- unsupported type: TyApp VECTOR []
  , "indexOffAddrAs#"                        -- unsupported type: TyApp VECTOR []
  , "readOffAddrAs#"                         -- unsupported type: TyApp VECTOR []
  , "writeOffAddrAs#"                        -- unsupported type: TyApp VECTOR []
  ]
