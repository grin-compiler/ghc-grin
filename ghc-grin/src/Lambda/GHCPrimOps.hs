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
    "gtChar#" :: (T_Char) @ t_0 -> (T_Char) @ t_1 -> (T_Int64) @ t_2
    "geChar#" :: (T_Char) @ t_3 -> (T_Char) @ t_4 -> (T_Int64) @ t_5
    "eqChar#" :: (T_Char) @ t_6 -> (T_Char) @ t_7 -> (T_Int64) @ t_8
    "neChar#" :: (T_Char) @ t_9 -> (T_Char) @ t_10 -> (T_Int64) @ t_11
    "ltChar#" :: (T_Char) @ t_12 -> (T_Char) @ t_13 -> (T_Int64) @ t_14
    "leChar#" :: (T_Char) @ t_15 -> (T_Char) @ t_16 -> (T_Int64) @ t_17
    "ord#"    :: (T_Char) @ t_18 -> (T_Int64) @ t_19

  {-
    Int8#
  -}
  primop pure
    "extendInt8#"  :: {"Int8#"} @ t_20 -> (T_Int64) @ t_21
    "narrowInt8#"  :: (T_Int64) @ t_22 -> {"Int8#"} @ t_23
    "negateInt8#"  :: {"Int8#"} @ t_24 -> {"Int8#"} @ t_25
    "plusInt8#"    :: {"Int8#"} @ t_26 -> {"Int8#"} @ t_27 -> {"Int8#"} @ t_28
    "subInt8#"     :: {"Int8#"} @ t_29 -> {"Int8#"} @ t_30 -> {"Int8#"} @ t_31
    "timesInt8#"   :: {"Int8#"} @ t_32 -> {"Int8#"} @ t_33 -> {"Int8#"} @ t_34
    "quotInt8#"    :: {"Int8#"} @ t_35 -> {"Int8#"} @ t_36 -> {"Int8#"} @ t_37
    "remInt8#"     :: {"Int8#"} @ t_38 -> {"Int8#"} @ t_39 -> {"Int8#"} @ t_40
    "quotRemInt8#" :: {"Int8#"} @ t_41 -> {"Int8#"} @ t_42 -> {"ghc-prim_GHC.Prim.(#,#)" {"Int8#"} @ t_43 {"Int8#"} @ t_44} @ t_45
    "eqInt8#"      :: {"Int8#"} @ t_46 -> {"Int8#"} @ t_47 -> (T_Int64) @ t_48
    "geInt8#"      :: {"Int8#"} @ t_49 -> {"Int8#"} @ t_50 -> (T_Int64) @ t_51
    "gtInt8#"      :: {"Int8#"} @ t_52 -> {"Int8#"} @ t_53 -> (T_Int64) @ t_54
    "leInt8#"      :: {"Int8#"} @ t_55 -> {"Int8#"} @ t_56 -> (T_Int64) @ t_57
    "ltInt8#"      :: {"Int8#"} @ t_58 -> {"Int8#"} @ t_59 -> (T_Int64) @ t_60
    "neInt8#"      :: {"Int8#"} @ t_61 -> {"Int8#"} @ t_62 -> (T_Int64) @ t_63

  {-
    Word8#
  -}
  primop pure
    "extendWord8#"  :: {"Word8#"} @ t_64 -> (T_Word64) @ t_65
    "narrowWord8#"  :: (T_Word64) @ t_66 -> {"Word8#"} @ t_67
    "notWord8#"     :: {"Word8#"} @ t_68 -> {"Word8#"} @ t_69
    "plusWord8#"    :: {"Word8#"} @ t_70 -> {"Word8#"} @ t_71 -> {"Word8#"} @ t_72
    "subWord8#"     :: {"Word8#"} @ t_73 -> {"Word8#"} @ t_74 -> {"Word8#"} @ t_75
    "timesWord8#"   :: {"Word8#"} @ t_76 -> {"Word8#"} @ t_77 -> {"Word8#"} @ t_78
    "quotWord8#"    :: {"Word8#"} @ t_79 -> {"Word8#"} @ t_80 -> {"Word8#"} @ t_81
    "remWord8#"     :: {"Word8#"} @ t_82 -> {"Word8#"} @ t_83 -> {"Word8#"} @ t_84
    "quotRemWord8#" :: {"Word8#"} @ t_85 -> {"Word8#"} @ t_86 -> {"ghc-prim_GHC.Prim.(#,#)" {"Word8#"} @ t_87 {"Word8#"} @ t_88} @ t_89
    "eqWord8#"      :: {"Word8#"} @ t_90 -> {"Word8#"} @ t_91 -> (T_Int64) @ t_92
    "geWord8#"      :: {"Word8#"} @ t_93 -> {"Word8#"} @ t_94 -> (T_Int64) @ t_95
    "gtWord8#"      :: {"Word8#"} @ t_96 -> {"Word8#"} @ t_97 -> (T_Int64) @ t_98
    "leWord8#"      :: {"Word8#"} @ t_99 -> {"Word8#"} @ t_100 -> (T_Int64) @ t_101
    "ltWord8#"      :: {"Word8#"} @ t_102 -> {"Word8#"} @ t_103 -> (T_Int64) @ t_104
    "neWord8#"      :: {"Word8#"} @ t_105 -> {"Word8#"} @ t_106 -> (T_Int64) @ t_107

  {-
    Int16#
  -}
  primop pure
    "extendInt16#"  :: {"Int16#"} @ t_108 -> (T_Int64) @ t_109
    "narrowInt16#"  :: (T_Int64) @ t_110 -> {"Int16#"} @ t_111
    "negateInt16#"  :: {"Int16#"} @ t_112 -> {"Int16#"} @ t_113
    "plusInt16#"    :: {"Int16#"} @ t_114 -> {"Int16#"} @ t_115 -> {"Int16#"} @ t_116
    "subInt16#"     :: {"Int16#"} @ t_117 -> {"Int16#"} @ t_118 -> {"Int16#"} @ t_119
    "timesInt16#"   :: {"Int16#"} @ t_120 -> {"Int16#"} @ t_121 -> {"Int16#"} @ t_122
    "quotInt16#"    :: {"Int16#"} @ t_123 -> {"Int16#"} @ t_124 -> {"Int16#"} @ t_125
    "remInt16#"     :: {"Int16#"} @ t_126 -> {"Int16#"} @ t_127 -> {"Int16#"} @ t_128
    "quotRemInt16#" :: {"Int16#"} @ t_129 -> {"Int16#"} @ t_130 -> {"ghc-prim_GHC.Prim.(#,#)" {"Int16#"} @ t_131 {"Int16#"} @ t_132} @ t_133
    "eqInt16#"      :: {"Int16#"} @ t_134 -> {"Int16#"} @ t_135 -> (T_Int64) @ t_136
    "geInt16#"      :: {"Int16#"} @ t_137 -> {"Int16#"} @ t_138 -> (T_Int64) @ t_139
    "gtInt16#"      :: {"Int16#"} @ t_140 -> {"Int16#"} @ t_141 -> (T_Int64) @ t_142
    "leInt16#"      :: {"Int16#"} @ t_143 -> {"Int16#"} @ t_144 -> (T_Int64) @ t_145
    "ltInt16#"      :: {"Int16#"} @ t_146 -> {"Int16#"} @ t_147 -> (T_Int64) @ t_148
    "neInt16#"      :: {"Int16#"} @ t_149 -> {"Int16#"} @ t_150 -> (T_Int64) @ t_151

  {-
    Word16#
  -}
  primop pure
    "extendWord16#"  :: {"Word16#"} @ t_152 -> (T_Word64) @ t_153
    "narrowWord16#"  :: (T_Word64) @ t_154 -> {"Word16#"} @ t_155
    "notWord16#"     :: {"Word16#"} @ t_156 -> {"Word16#"} @ t_157
    "plusWord16#"    :: {"Word16#"} @ t_158 -> {"Word16#"} @ t_159 -> {"Word16#"} @ t_160
    "subWord16#"     :: {"Word16#"} @ t_161 -> {"Word16#"} @ t_162 -> {"Word16#"} @ t_163
    "timesWord16#"   :: {"Word16#"} @ t_164 -> {"Word16#"} @ t_165 -> {"Word16#"} @ t_166
    "quotWord16#"    :: {"Word16#"} @ t_167 -> {"Word16#"} @ t_168 -> {"Word16#"} @ t_169
    "remWord16#"     :: {"Word16#"} @ t_170 -> {"Word16#"} @ t_171 -> {"Word16#"} @ t_172
    "quotRemWord16#" :: {"Word16#"} @ t_173 -> {"Word16#"} @ t_174 -> {"ghc-prim_GHC.Prim.(#,#)" {"Word16#"} @ t_175 {"Word16#"} @ t_176} @ t_177
    "eqWord16#"      :: {"Word16#"} @ t_178 -> {"Word16#"} @ t_179 -> (T_Int64) @ t_180
    "geWord16#"      :: {"Word16#"} @ t_181 -> {"Word16#"} @ t_182 -> (T_Int64) @ t_183
    "gtWord16#"      :: {"Word16#"} @ t_184 -> {"Word16#"} @ t_185 -> (T_Int64) @ t_186
    "leWord16#"      :: {"Word16#"} @ t_187 -> {"Word16#"} @ t_188 -> (T_Int64) @ t_189
    "ltWord16#"      :: {"Word16#"} @ t_190 -> {"Word16#"} @ t_191 -> (T_Int64) @ t_192
    "neWord16#"      :: {"Word16#"} @ t_193 -> {"Word16#"} @ t_194 -> (T_Int64) @ t_195

  {-
    Int#
  -}
  primop pure
    "+#"                 :: (T_Int64) @ t_196 -> (T_Int64) @ t_197 -> (T_Int64) @ t_198
    "-#"                 :: (T_Int64) @ t_199 -> (T_Int64) @ t_200 -> (T_Int64) @ t_201
    "*#"                 :: (T_Int64) @ t_202 -> (T_Int64) @ t_203 -> (T_Int64) @ t_204
    "timesInt2#"         :: (T_Int64) @ t_205 -> (T_Int64) @ t_206 -> {"ghc-prim_GHC.Prim.(#,,#)" (T_Int64) @ t_207 (T_Int64) @ t_208 (T_Int64) @ t_209} @ t_210
    "mulIntMayOflo#"     :: (T_Int64) @ t_211 -> (T_Int64) @ t_212 -> (T_Int64) @ t_213
    "quotInt#"           :: (T_Int64) @ t_214 -> (T_Int64) @ t_215 -> (T_Int64) @ t_216
    "remInt#"            :: (T_Int64) @ t_217 -> (T_Int64) @ t_218 -> (T_Int64) @ t_219
    "quotRemInt#"        :: (T_Int64) @ t_220 -> (T_Int64) @ t_221 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_222 (T_Int64) @ t_223} @ t_224
    "andI#"              :: (T_Int64) @ t_225 -> (T_Int64) @ t_226 -> (T_Int64) @ t_227
    "orI#"               :: (T_Int64) @ t_228 -> (T_Int64) @ t_229 -> (T_Int64) @ t_230
    "xorI#"              :: (T_Int64) @ t_231 -> (T_Int64) @ t_232 -> (T_Int64) @ t_233
    "notI#"              :: (T_Int64) @ t_234 -> (T_Int64) @ t_235
    "negateInt#"         :: (T_Int64) @ t_236 -> (T_Int64) @ t_237
    "addIntC#"           :: (T_Int64) @ t_238 -> (T_Int64) @ t_239 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_240 (T_Int64) @ t_241} @ t_242
    "subIntC#"           :: (T_Int64) @ t_243 -> (T_Int64) @ t_244 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_245 (T_Int64) @ t_246} @ t_247
    ">#"                 :: (T_Int64) @ t_248 -> (T_Int64) @ t_249 -> (T_Int64) @ t_250
    ">=#"                :: (T_Int64) @ t_251 -> (T_Int64) @ t_252 -> (T_Int64) @ t_253
    "==#"                :: (T_Int64) @ t_254 -> (T_Int64) @ t_255 -> (T_Int64) @ t_256
    "/=#"                :: (T_Int64) @ t_257 -> (T_Int64) @ t_258 -> (T_Int64) @ t_259
    "<#"                 :: (T_Int64) @ t_260 -> (T_Int64) @ t_261 -> (T_Int64) @ t_262
    "<=#"                :: (T_Int64) @ t_263 -> (T_Int64) @ t_264 -> (T_Int64) @ t_265
    "chr#"               :: (T_Int64) @ t_266 -> (T_Char) @ t_267
    "int2Word#"          :: (T_Int64) @ t_268 -> (T_Word64) @ t_269
    "int2Float#"         :: (T_Int64) @ t_270 -> (T_Float) @ t_271
    "int2Double#"        :: (T_Int64) @ t_272 -> (T_Double) @ t_273
    "word2Float#"        :: (T_Word64) @ t_274 -> (T_Float) @ t_275
    "word2Double#"       :: (T_Word64) @ t_276 -> (T_Double) @ t_277
    "uncheckedIShiftL#"  :: (T_Int64) @ t_278 -> (T_Int64) @ t_279 -> (T_Int64) @ t_280
    "uncheckedIShiftRA#" :: (T_Int64) @ t_281 -> (T_Int64) @ t_282 -> (T_Int64) @ t_283
    "uncheckedIShiftRL#" :: (T_Int64) @ t_284 -> (T_Int64) @ t_285 -> (T_Int64) @ t_286

  {-
    Word#
  -}
  primop pure
    "plusWord#"         :: (T_Word64) @ t_287 -> (T_Word64) @ t_288 -> (T_Word64) @ t_289
    "addWordC#"         :: (T_Word64) @ t_290 -> (T_Word64) @ t_291 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Word64) @ t_292 (T_Int64) @ t_293} @ t_294
    "subWordC#"         :: (T_Word64) @ t_295 -> (T_Word64) @ t_296 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Word64) @ t_297 (T_Int64) @ t_298} @ t_299
    "plusWord2#"        :: (T_Word64) @ t_300 -> (T_Word64) @ t_301 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Word64) @ t_302 (T_Word64) @ t_303} @ t_304
    "minusWord#"        :: (T_Word64) @ t_305 -> (T_Word64) @ t_306 -> (T_Word64) @ t_307
    "timesWord#"        :: (T_Word64) @ t_308 -> (T_Word64) @ t_309 -> (T_Word64) @ t_310
    "timesWord2#"       :: (T_Word64) @ t_311 -> (T_Word64) @ t_312 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Word64) @ t_313 (T_Word64) @ t_314} @ t_315
    "quotWord#"         :: (T_Word64) @ t_316 -> (T_Word64) @ t_317 -> (T_Word64) @ t_318
    "remWord#"          :: (T_Word64) @ t_319 -> (T_Word64) @ t_320 -> (T_Word64) @ t_321
    "quotRemWord#"      :: (T_Word64) @ t_322 -> (T_Word64) @ t_323 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Word64) @ t_324 (T_Word64) @ t_325} @ t_326
    "quotRemWord2#"     :: (T_Word64) @ t_327 -> (T_Word64) @ t_328 -> (T_Word64) @ t_329 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Word64) @ t_330 (T_Word64) @ t_331} @ t_332
    "and#"              :: (T_Word64) @ t_333 -> (T_Word64) @ t_334 -> (T_Word64) @ t_335
    "or#"               :: (T_Word64) @ t_336 -> (T_Word64) @ t_337 -> (T_Word64) @ t_338
    "xor#"              :: (T_Word64) @ t_339 -> (T_Word64) @ t_340 -> (T_Word64) @ t_341
    "not#"              :: (T_Word64) @ t_342 -> (T_Word64) @ t_343
    "uncheckedShiftL#"  :: (T_Word64) @ t_344 -> (T_Int64) @ t_345 -> (T_Word64) @ t_346
    "uncheckedShiftRL#" :: (T_Word64) @ t_347 -> (T_Int64) @ t_348 -> (T_Word64) @ t_349
    "word2Int#"         :: (T_Word64) @ t_350 -> (T_Int64) @ t_351
    "gtWord#"           :: (T_Word64) @ t_352 -> (T_Word64) @ t_353 -> (T_Int64) @ t_354
    "geWord#"           :: (T_Word64) @ t_355 -> (T_Word64) @ t_356 -> (T_Int64) @ t_357
    "eqWord#"           :: (T_Word64) @ t_358 -> (T_Word64) @ t_359 -> (T_Int64) @ t_360
    "neWord#"           :: (T_Word64) @ t_361 -> (T_Word64) @ t_362 -> (T_Int64) @ t_363
    "ltWord#"           :: (T_Word64) @ t_364 -> (T_Word64) @ t_365 -> (T_Int64) @ t_366
    "leWord#"           :: (T_Word64) @ t_367 -> (T_Word64) @ t_368 -> (T_Int64) @ t_369
    "popCnt8#"          :: (T_Word64) @ t_370 -> (T_Word64) @ t_371
    "popCnt16#"         :: (T_Word64) @ t_372 -> (T_Word64) @ t_373
    "popCnt32#"         :: (T_Word64) @ t_374 -> (T_Word64) @ t_375
    "popCnt64#"         :: (T_Word64) @ t_376 -> (T_Word64) @ t_377
    "popCnt#"           :: (T_Word64) @ t_378 -> (T_Word64) @ t_379
    "pdep8#"            :: (T_Word64) @ t_380 -> (T_Word64) @ t_381 -> (T_Word64) @ t_382
    "pdep16#"           :: (T_Word64) @ t_383 -> (T_Word64) @ t_384 -> (T_Word64) @ t_385
    "pdep32#"           :: (T_Word64) @ t_386 -> (T_Word64) @ t_387 -> (T_Word64) @ t_388
    "pdep64#"           :: (T_Word64) @ t_389 -> (T_Word64) @ t_390 -> (T_Word64) @ t_391
    "pdep#"             :: (T_Word64) @ t_392 -> (T_Word64) @ t_393 -> (T_Word64) @ t_394
    "pext8#"            :: (T_Word64) @ t_395 -> (T_Word64) @ t_396 -> (T_Word64) @ t_397
    "pext16#"           :: (T_Word64) @ t_398 -> (T_Word64) @ t_399 -> (T_Word64) @ t_400
    "pext32#"           :: (T_Word64) @ t_401 -> (T_Word64) @ t_402 -> (T_Word64) @ t_403
    "pext64#"           :: (T_Word64) @ t_404 -> (T_Word64) @ t_405 -> (T_Word64) @ t_406
    "pext#"             :: (T_Word64) @ t_407 -> (T_Word64) @ t_408 -> (T_Word64) @ t_409
    "clz8#"             :: (T_Word64) @ t_410 -> (T_Word64) @ t_411
    "clz16#"            :: (T_Word64) @ t_412 -> (T_Word64) @ t_413
    "clz32#"            :: (T_Word64) @ t_414 -> (T_Word64) @ t_415
    "clz64#"            :: (T_Word64) @ t_416 -> (T_Word64) @ t_417
    "clz#"              :: (T_Word64) @ t_418 -> (T_Word64) @ t_419
    "ctz8#"             :: (T_Word64) @ t_420 -> (T_Word64) @ t_421
    "ctz16#"            :: (T_Word64) @ t_422 -> (T_Word64) @ t_423
    "ctz32#"            :: (T_Word64) @ t_424 -> (T_Word64) @ t_425
    "ctz64#"            :: (T_Word64) @ t_426 -> (T_Word64) @ t_427
    "ctz#"              :: (T_Word64) @ t_428 -> (T_Word64) @ t_429
    "byteSwap16#"       :: (T_Word64) @ t_430 -> (T_Word64) @ t_431
    "byteSwap32#"       :: (T_Word64) @ t_432 -> (T_Word64) @ t_433
    "byteSwap64#"       :: (T_Word64) @ t_434 -> (T_Word64) @ t_435
    "byteSwap#"         :: (T_Word64) @ t_436 -> (T_Word64) @ t_437
    "bitReverse8#"      :: (T_Word64) @ t_438 -> (T_Word64) @ t_439
    "bitReverse16#"     :: (T_Word64) @ t_440 -> (T_Word64) @ t_441
    "bitReverse32#"     :: (T_Word64) @ t_442 -> (T_Word64) @ t_443
    "bitReverse64#"     :: (T_Word64) @ t_444 -> (T_Word64) @ t_445
    "bitReverse#"       :: (T_Word64) @ t_446 -> (T_Word64) @ t_447

  {-
    Narrowings
  -}
  primop pure
    "narrow8Int#"   :: (T_Int64) @ t_448 -> (T_Int64) @ t_449
    "narrow16Int#"  :: (T_Int64) @ t_450 -> (T_Int64) @ t_451
    "narrow32Int#"  :: (T_Int64) @ t_452 -> (T_Int64) @ t_453
    "narrow8Word#"  :: (T_Word64) @ t_454 -> (T_Word64) @ t_455
    "narrow16Word#" :: (T_Word64) @ t_456 -> (T_Word64) @ t_457
    "narrow32Word#" :: (T_Word64) @ t_458 -> (T_Word64) @ t_459

  {-
    Double#
  -}
  primop pure
    ">##"                 :: (T_Double) @ t_460 -> (T_Double) @ t_461 -> (T_Int64) @ t_462
    ">=##"                :: (T_Double) @ t_463 -> (T_Double) @ t_464 -> (T_Int64) @ t_465
    "==##"                :: (T_Double) @ t_466 -> (T_Double) @ t_467 -> (T_Int64) @ t_468
    "/=##"                :: (T_Double) @ t_469 -> (T_Double) @ t_470 -> (T_Int64) @ t_471
    "<##"                 :: (T_Double) @ t_472 -> (T_Double) @ t_473 -> (T_Int64) @ t_474
    "<=##"                :: (T_Double) @ t_475 -> (T_Double) @ t_476 -> (T_Int64) @ t_477
    "+##"                 :: (T_Double) @ t_478 -> (T_Double) @ t_479 -> (T_Double) @ t_480
    "-##"                 :: (T_Double) @ t_481 -> (T_Double) @ t_482 -> (T_Double) @ t_483
    "*##"                 :: (T_Double) @ t_484 -> (T_Double) @ t_485 -> (T_Double) @ t_486
    "/##"                 :: (T_Double) @ t_487 -> (T_Double) @ t_488 -> (T_Double) @ t_489
    "negateDouble#"       :: (T_Double) @ t_490 -> (T_Double) @ t_491
    "fabsDouble#"         :: (T_Double) @ t_492 -> (T_Double) @ t_493
    "double2Int#"         :: (T_Double) @ t_494 -> (T_Int64) @ t_495
    "double2Float#"       :: (T_Double) @ t_496 -> (T_Float) @ t_497
    "expDouble#"          :: (T_Double) @ t_498 -> (T_Double) @ t_499
    "expm1Double#"        :: (T_Double) @ t_500 -> (T_Double) @ t_501
    "logDouble#"          :: (T_Double) @ t_502 -> (T_Double) @ t_503
    "log1pDouble#"        :: (T_Double) @ t_504 -> (T_Double) @ t_505
    "sqrtDouble#"         :: (T_Double) @ t_506 -> (T_Double) @ t_507
    "sinDouble#"          :: (T_Double) @ t_508 -> (T_Double) @ t_509
    "cosDouble#"          :: (T_Double) @ t_510 -> (T_Double) @ t_511
    "tanDouble#"          :: (T_Double) @ t_512 -> (T_Double) @ t_513
    "asinDouble#"         :: (T_Double) @ t_514 -> (T_Double) @ t_515
    "acosDouble#"         :: (T_Double) @ t_516 -> (T_Double) @ t_517
    "atanDouble#"         :: (T_Double) @ t_518 -> (T_Double) @ t_519
    "sinhDouble#"         :: (T_Double) @ t_520 -> (T_Double) @ t_521
    "coshDouble#"         :: (T_Double) @ t_522 -> (T_Double) @ t_523
    "tanhDouble#"         :: (T_Double) @ t_524 -> (T_Double) @ t_525
    "asinhDouble#"        :: (T_Double) @ t_526 -> (T_Double) @ t_527
    "acoshDouble#"        :: (T_Double) @ t_528 -> (T_Double) @ t_529
    "atanhDouble#"        :: (T_Double) @ t_530 -> (T_Double) @ t_531
    "**##"                :: (T_Double) @ t_532 -> (T_Double) @ t_533 -> (T_Double) @ t_534
    "decodeDouble_2Int#"  :: (T_Double) @ t_535 -> {"ghc-prim_GHC.Prim.(#,,,#)" (T_Int64) @ t_536 (T_Word64) @ t_537 (T_Word64) @ t_538 (T_Int64) @ t_539} @ t_540
    "decodeDouble_Int64#" :: (T_Double) @ t_541 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_542 (T_Int64) @ t_543} @ t_544

  {-
    Float#
  -}
  primop pure
    "gtFloat#"         :: (T_Float) @ t_545 -> (T_Float) @ t_546 -> (T_Int64) @ t_547
    "geFloat#"         :: (T_Float) @ t_548 -> (T_Float) @ t_549 -> (T_Int64) @ t_550
    "eqFloat#"         :: (T_Float) @ t_551 -> (T_Float) @ t_552 -> (T_Int64) @ t_553
    "neFloat#"         :: (T_Float) @ t_554 -> (T_Float) @ t_555 -> (T_Int64) @ t_556
    "ltFloat#"         :: (T_Float) @ t_557 -> (T_Float) @ t_558 -> (T_Int64) @ t_559
    "leFloat#"         :: (T_Float) @ t_560 -> (T_Float) @ t_561 -> (T_Int64) @ t_562
    "plusFloat#"       :: (T_Float) @ t_563 -> (T_Float) @ t_564 -> (T_Float) @ t_565
    "minusFloat#"      :: (T_Float) @ t_566 -> (T_Float) @ t_567 -> (T_Float) @ t_568
    "timesFloat#"      :: (T_Float) @ t_569 -> (T_Float) @ t_570 -> (T_Float) @ t_571
    "divideFloat#"     :: (T_Float) @ t_572 -> (T_Float) @ t_573 -> (T_Float) @ t_574
    "negateFloat#"     :: (T_Float) @ t_575 -> (T_Float) @ t_576
    "fabsFloat#"       :: (T_Float) @ t_577 -> (T_Float) @ t_578
    "float2Int#"       :: (T_Float) @ t_579 -> (T_Int64) @ t_580
    "expFloat#"        :: (T_Float) @ t_581 -> (T_Float) @ t_582
    "expm1Float#"      :: (T_Float) @ t_583 -> (T_Float) @ t_584
    "logFloat#"        :: (T_Float) @ t_585 -> (T_Float) @ t_586
    "log1pFloat#"      :: (T_Float) @ t_587 -> (T_Float) @ t_588
    "sqrtFloat#"       :: (T_Float) @ t_589 -> (T_Float) @ t_590
    "sinFloat#"        :: (T_Float) @ t_591 -> (T_Float) @ t_592
    "cosFloat#"        :: (T_Float) @ t_593 -> (T_Float) @ t_594
    "tanFloat#"        :: (T_Float) @ t_595 -> (T_Float) @ t_596
    "asinFloat#"       :: (T_Float) @ t_597 -> (T_Float) @ t_598
    "acosFloat#"       :: (T_Float) @ t_599 -> (T_Float) @ t_600
    "atanFloat#"       :: (T_Float) @ t_601 -> (T_Float) @ t_602
    "sinhFloat#"       :: (T_Float) @ t_603 -> (T_Float) @ t_604
    "coshFloat#"       :: (T_Float) @ t_605 -> (T_Float) @ t_606
    "tanhFloat#"       :: (T_Float) @ t_607 -> (T_Float) @ t_608
    "asinhFloat#"      :: (T_Float) @ t_609 -> (T_Float) @ t_610
    "acoshFloat#"      :: (T_Float) @ t_611 -> (T_Float) @ t_612
    "atanhFloat#"      :: (T_Float) @ t_613 -> (T_Float) @ t_614
    "powerFloat#"      :: (T_Float) @ t_615 -> (T_Float) @ t_616 -> (T_Float) @ t_617
    "float2Double#"    :: (T_Float) @ t_618 -> (T_Double) @ t_619
    "decodeFloat_Int#" :: (T_Float) @ t_620 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_621 (T_Int64) @ t_622} @ t_623

  {-
    Arrays
  -}
  primop effectful
    "newArray#" :: (T_Int64) @ t_624 -> %a_0 -> {"State#" %s_0} @ t_625 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableArray#" %s_0 %a_0} @ t_626} @ t_627

  primop pure
    "sameMutableArray#" :: {"MutableArray#" %s_1 %a_1} @ t_628 -> {"MutableArray#" %s_1 %a_1} @ t_629 -> (T_Int64) @ t_630

  primop effectful
    "readArray#"  :: {"MutableArray#" %s_2 %a_2} @ t_631 -> (T_Int64) @ t_632 -> {"State#" %s_2} @ t_633 -> {"ghc-prim_GHC.Prim.Unit#" %a_2} @ t_634
    "writeArray#" :: {"MutableArray#" %s_3 %a_3} @ t_635 -> (T_Int64) @ t_636 -> %a_3 -> {"State#" %s_3} @ t_637 -> {"ghc-prim_GHC.Prim.(##)"} @ t_638

  primop pure
    "sizeofArray#"        :: {"Array#" %a_4} @ t_639 -> (T_Int64) @ t_640
    "sizeofMutableArray#" :: {"MutableArray#" %s_4 %a_5} @ t_641 -> (T_Int64) @ t_642
    "indexArray#"         :: {"Array#" %a_6} @ t_643 -> (T_Int64) @ t_644 -> {"ghc-prim_GHC.Prim.Unit#" %a_6} @ t_645

  primop effectful
    "unsafeFreezeArray#" :: {"MutableArray#" %s_5 %a_7} @ t_646 -> {"State#" %s_5} @ t_647 -> {"ghc-prim_GHC.Prim.Unit#" {"Array#" %a_7} @ t_648} @ t_649
    "unsafeThawArray#"   :: {"Array#" %a_8} @ t_650 -> {"State#" %s_6} @ t_651 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableArray#" %s_6 %a_8} @ t_652} @ t_653
    "copyArray#"         :: {"Array#" %a_9} @ t_654 -> (T_Int64) @ t_655 -> {"MutableArray#" %s_7 %a_9} @ t_656 -> (T_Int64) @ t_657 -> (T_Int64) @ t_658 -> {"State#" %s_7} @ t_659 -> {"ghc-prim_GHC.Prim.(##)"} @ t_660
    "copyMutableArray#"  :: {"MutableArray#" %s_8 %a_10} @ t_661 -> (T_Int64) @ t_662 -> {"MutableArray#" %s_8 %a_10} @ t_663 -> (T_Int64) @ t_664 -> (T_Int64) @ t_665 -> {"State#" %s_8} @ t_666 -> {"ghc-prim_GHC.Prim.(##)"} @ t_667
    "cloneArray#"        :: {"Array#" %a_11} @ t_668 -> (T_Int64) @ t_669 -> (T_Int64) @ t_670 -> {"Array#" %a_11} @ t_671
    "cloneMutableArray#" :: {"MutableArray#" %s_9 %a_12} @ t_672 -> (T_Int64) @ t_673 -> (T_Int64) @ t_674 -> {"State#" %s_9} @ t_675 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableArray#" %s_9 %a_12} @ t_676} @ t_677
    "freezeArray#"       :: {"MutableArray#" %s_10 %a_13} @ t_678 -> (T_Int64) @ t_679 -> (T_Int64) @ t_680 -> {"State#" %s_10} @ t_681 -> {"ghc-prim_GHC.Prim.Unit#" {"Array#" %a_13} @ t_682} @ t_683
    "thawArray#"         :: {"Array#" %a_14} @ t_684 -> (T_Int64) @ t_685 -> (T_Int64) @ t_686 -> {"State#" %s_11} @ t_687 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableArray#" %s_11 %a_14} @ t_688} @ t_689
    "casArray#"          :: {"MutableArray#" %s_12 %a_15} @ t_690 -> (T_Int64) @ t_691 -> %a_15 -> %a_15 -> {"State#" %s_12} @ t_692 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_693 %a_15} @ t_694

  {-
    Small Arrays
  -}
  primop effectful
    "newSmallArray#" :: (T_Int64) @ t_695 -> %a_16 -> {"State#" %s_13} @ t_696 -> {"ghc-prim_GHC.Prim.Unit#" {"SmallMutableArray#" %s_13 %a_16} @ t_697} @ t_698

  primop pure
    "sameSmallMutableArray#" :: {"SmallMutableArray#" %s_14 %a_17} @ t_699 -> {"SmallMutableArray#" %s_14 %a_17} @ t_700 -> (T_Int64) @ t_701

  primop effectful
    "shrinkSmallMutableArray#" :: {"SmallMutableArray#" %s_15 %a_18} @ t_702 -> (T_Int64) @ t_703 -> {"State#" %s_15} @ t_704 -> {"ghc-prim_GHC.Prim.(##)"} @ t_705
    "readSmallArray#"          :: {"SmallMutableArray#" %s_16 %a_19} @ t_706 -> (T_Int64) @ t_707 -> {"State#" %s_16} @ t_708 -> {"ghc-prim_GHC.Prim.Unit#" %a_19} @ t_709
    "writeSmallArray#"         :: {"SmallMutableArray#" %s_17 %a_20} @ t_710 -> (T_Int64) @ t_711 -> %a_20 -> {"State#" %s_17} @ t_712 -> {"ghc-prim_GHC.Prim.(##)"} @ t_713

  primop pure
    "sizeofSmallArray#"           :: {"SmallArray#" %a_21} @ t_714 -> (T_Int64) @ t_715
    "sizeofSmallMutableArray#"    :: {"SmallMutableArray#" %s_18 %a_22} @ t_716 -> (T_Int64) @ t_717
    "getSizeofSmallMutableArray#" :: {"SmallMutableArray#" %s_19 %a_23} @ t_718 -> {"State#" %s_19} @ t_719 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_720} @ t_721
    "indexSmallArray#"            :: {"SmallArray#" %a_24} @ t_722 -> (T_Int64) @ t_723 -> {"ghc-prim_GHC.Prim.Unit#" %a_24} @ t_724

  primop effectful
    "unsafeFreezeSmallArray#" :: {"SmallMutableArray#" %s_20 %a_25} @ t_725 -> {"State#" %s_20} @ t_726 -> {"ghc-prim_GHC.Prim.Unit#" {"SmallArray#" %a_25} @ t_727} @ t_728
    "unsafeThawSmallArray#"   :: {"SmallArray#" %a_26} @ t_729 -> {"State#" %s_21} @ t_730 -> {"ghc-prim_GHC.Prim.Unit#" {"SmallMutableArray#" %s_21 %a_26} @ t_731} @ t_732
    "copySmallArray#"         :: {"SmallArray#" %a_27} @ t_733 -> (T_Int64) @ t_734 -> {"SmallMutableArray#" %s_22 %a_27} @ t_735 -> (T_Int64) @ t_736 -> (T_Int64) @ t_737 -> {"State#" %s_22} @ t_738 -> {"ghc-prim_GHC.Prim.(##)"} @ t_739
    "copySmallMutableArray#"  :: {"SmallMutableArray#" %s_23 %a_28} @ t_740 -> (T_Int64) @ t_741 -> {"SmallMutableArray#" %s_23 %a_28} @ t_742 -> (T_Int64) @ t_743 -> (T_Int64) @ t_744 -> {"State#" %s_23} @ t_745 -> {"ghc-prim_GHC.Prim.(##)"} @ t_746
    "cloneSmallArray#"        :: {"SmallArray#" %a_29} @ t_747 -> (T_Int64) @ t_748 -> (T_Int64) @ t_749 -> {"SmallArray#" %a_29} @ t_750
    "cloneSmallMutableArray#" :: {"SmallMutableArray#" %s_24 %a_30} @ t_751 -> (T_Int64) @ t_752 -> (T_Int64) @ t_753 -> {"State#" %s_24} @ t_754 -> {"ghc-prim_GHC.Prim.Unit#" {"SmallMutableArray#" %s_24 %a_30} @ t_755} @ t_756
    "freezeSmallArray#"       :: {"SmallMutableArray#" %s_25 %a_31} @ t_757 -> (T_Int64) @ t_758 -> (T_Int64) @ t_759 -> {"State#" %s_25} @ t_760 -> {"ghc-prim_GHC.Prim.Unit#" {"SmallArray#" %a_31} @ t_761} @ t_762
    "thawSmallArray#"         :: {"SmallArray#" %a_32} @ t_763 -> (T_Int64) @ t_764 -> (T_Int64) @ t_765 -> {"State#" %s_26} @ t_766 -> {"ghc-prim_GHC.Prim.Unit#" {"SmallMutableArray#" %s_26 %a_32} @ t_767} @ t_768
    "casSmallArray#"          :: {"SmallMutableArray#" %s_27 %a_33} @ t_769 -> (T_Int64) @ t_770 -> %a_33 -> %a_33 -> {"State#" %s_27} @ t_771 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_772 %a_33} @ t_773

  {-
    Byte Arrays
  -}
  primop effectful
    "newByteArray#"              :: (T_Int64) @ t_774 -> {"State#" %s_28} @ t_775 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableByteArray#" %s_28} @ t_776} @ t_777
    "newPinnedByteArray#"        :: (T_Int64) @ t_778 -> {"State#" %s_29} @ t_779 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableByteArray#" %s_29} @ t_780} @ t_781
    "newAlignedPinnedByteArray#" :: (T_Int64) @ t_782 -> (T_Int64) @ t_783 -> {"State#" %s_30} @ t_784 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableByteArray#" %s_30} @ t_785} @ t_786

  primop pure
    "isMutableByteArrayPinned#" :: {"MutableByteArray#" %s_31} @ t_787 -> (T_Int64) @ t_788
    "isByteArrayPinned#"        :: {"ByteArray#"} @ t_789 -> (T_Int64) @ t_790
    "byteArrayContents#"        :: {"ByteArray#"} @ t_791 -> (T_Addr) @ t_792
    "sameMutableByteArray#"     :: {"MutableByteArray#" %s_32} @ t_793 -> {"MutableByteArray#" %s_32} @ t_794 -> (T_Int64) @ t_795

  primop effectful
    "shrinkMutableByteArray#" :: {"MutableByteArray#" %s_33} @ t_796 -> (T_Int64) @ t_797 -> {"State#" %s_33} @ t_798 -> {"ghc-prim_GHC.Prim.(##)"} @ t_799
    "resizeMutableByteArray#" :: {"MutableByteArray#" %s_34} @ t_800 -> (T_Int64) @ t_801 -> {"State#" %s_34} @ t_802 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableByteArray#" %s_34} @ t_803} @ t_804
    "unsafeFreezeByteArray#"  :: {"MutableByteArray#" %s_35} @ t_805 -> {"State#" %s_35} @ t_806 -> {"ghc-prim_GHC.Prim.Unit#" {"ByteArray#"} @ t_807} @ t_808

  primop pure
    "sizeofByteArray#"            :: {"ByteArray#"} @ t_809 -> (T_Int64) @ t_810
    "sizeofMutableByteArray#"     :: {"MutableByteArray#" %s_36} @ t_811 -> (T_Int64) @ t_812
    "getSizeofMutableByteArray#"  :: {"MutableByteArray#" %s_37} @ t_813 -> {"State#" %s_37} @ t_814 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_815} @ t_816
    "indexCharArray#"             :: {"ByteArray#"} @ t_817 -> (T_Int64) @ t_818 -> (T_Char) @ t_819
    "indexWideCharArray#"         :: {"ByteArray#"} @ t_820 -> (T_Int64) @ t_821 -> (T_Char) @ t_822
    "indexIntArray#"              :: {"ByteArray#"} @ t_823 -> (T_Int64) @ t_824 -> (T_Int64) @ t_825
    "indexWordArray#"             :: {"ByteArray#"} @ t_826 -> (T_Int64) @ t_827 -> (T_Word64) @ t_828
    "indexAddrArray#"             :: {"ByteArray#"} @ t_829 -> (T_Int64) @ t_830 -> (T_Addr) @ t_831
    "indexFloatArray#"            :: {"ByteArray#"} @ t_832 -> (T_Int64) @ t_833 -> (T_Float) @ t_834
    "indexDoubleArray#"           :: {"ByteArray#"} @ t_835 -> (T_Int64) @ t_836 -> (T_Double) @ t_837
    "indexStablePtrArray#"        :: {"ByteArray#"} @ t_838 -> (T_Int64) @ t_839 -> {"StablePtr#" %a_34} @ t_840
    "indexInt8Array#"             :: {"ByteArray#"} @ t_841 -> (T_Int64) @ t_842 -> (T_Int64) @ t_843
    "indexInt16Array#"            :: {"ByteArray#"} @ t_844 -> (T_Int64) @ t_845 -> (T_Int64) @ t_846
    "indexInt32Array#"            :: {"ByteArray#"} @ t_847 -> (T_Int64) @ t_848 -> (T_Int64) @ t_849
    "indexInt64Array#"            :: {"ByteArray#"} @ t_850 -> (T_Int64) @ t_851 -> (T_Int64) @ t_852
    "indexWord8Array#"            :: {"ByteArray#"} @ t_853 -> (T_Int64) @ t_854 -> (T_Word64) @ t_855
    "indexWord16Array#"           :: {"ByteArray#"} @ t_856 -> (T_Int64) @ t_857 -> (T_Word64) @ t_858
    "indexWord32Array#"           :: {"ByteArray#"} @ t_859 -> (T_Int64) @ t_860 -> (T_Word64) @ t_861
    "indexWord64Array#"           :: {"ByteArray#"} @ t_862 -> (T_Int64) @ t_863 -> (T_Word64) @ t_864
    "indexWord8ArrayAsChar#"      :: {"ByteArray#"} @ t_865 -> (T_Int64) @ t_866 -> (T_Char) @ t_867
    "indexWord8ArrayAsWideChar#"  :: {"ByteArray#"} @ t_868 -> (T_Int64) @ t_869 -> (T_Char) @ t_870
    "indexWord8ArrayAsAddr#"      :: {"ByteArray#"} @ t_871 -> (T_Int64) @ t_872 -> (T_Addr) @ t_873
    "indexWord8ArrayAsFloat#"     :: {"ByteArray#"} @ t_874 -> (T_Int64) @ t_875 -> (T_Float) @ t_876
    "indexWord8ArrayAsDouble#"    :: {"ByteArray#"} @ t_877 -> (T_Int64) @ t_878 -> (T_Double) @ t_879
    "indexWord8ArrayAsStablePtr#" :: {"ByteArray#"} @ t_880 -> (T_Int64) @ t_881 -> {"StablePtr#" %a_35} @ t_882
    "indexWord8ArrayAsInt16#"     :: {"ByteArray#"} @ t_883 -> (T_Int64) @ t_884 -> (T_Int64) @ t_885
    "indexWord8ArrayAsInt32#"     :: {"ByteArray#"} @ t_886 -> (T_Int64) @ t_887 -> (T_Int64) @ t_888
    "indexWord8ArrayAsInt64#"     :: {"ByteArray#"} @ t_889 -> (T_Int64) @ t_890 -> (T_Int64) @ t_891
    "indexWord8ArrayAsInt#"       :: {"ByteArray#"} @ t_892 -> (T_Int64) @ t_893 -> (T_Int64) @ t_894
    "indexWord8ArrayAsWord16#"    :: {"ByteArray#"} @ t_895 -> (T_Int64) @ t_896 -> (T_Word64) @ t_897
    "indexWord8ArrayAsWord32#"    :: {"ByteArray#"} @ t_898 -> (T_Int64) @ t_899 -> (T_Word64) @ t_900
    "indexWord8ArrayAsWord64#"    :: {"ByteArray#"} @ t_901 -> (T_Int64) @ t_902 -> (T_Word64) @ t_903
    "indexWord8ArrayAsWord#"      :: {"ByteArray#"} @ t_904 -> (T_Int64) @ t_905 -> (T_Word64) @ t_906

  primop effectful
    "readCharArray#"              :: {"MutableByteArray#" %s_38} @ t_907 -> (T_Int64) @ t_908 -> {"State#" %s_38} @ t_909 -> {"ghc-prim_GHC.Prim.Unit#" (T_Char) @ t_910} @ t_911
    "readWideCharArray#"          :: {"MutableByteArray#" %s_39} @ t_912 -> (T_Int64) @ t_913 -> {"State#" %s_39} @ t_914 -> {"ghc-prim_GHC.Prim.Unit#" (T_Char) @ t_915} @ t_916
    "readIntArray#"               :: {"MutableByteArray#" %s_40} @ t_917 -> (T_Int64) @ t_918 -> {"State#" %s_40} @ t_919 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_920} @ t_921
    "readWordArray#"              :: {"MutableByteArray#" %s_41} @ t_922 -> (T_Int64) @ t_923 -> {"State#" %s_41} @ t_924 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_925} @ t_926
    "readAddrArray#"              :: {"MutableByteArray#" %s_42} @ t_927 -> (T_Int64) @ t_928 -> {"State#" %s_42} @ t_929 -> {"ghc-prim_GHC.Prim.Unit#" (T_Addr) @ t_930} @ t_931
    "readFloatArray#"             :: {"MutableByteArray#" %s_43} @ t_932 -> (T_Int64) @ t_933 -> {"State#" %s_43} @ t_934 -> {"ghc-prim_GHC.Prim.Unit#" (T_Float) @ t_935} @ t_936
    "readDoubleArray#"            :: {"MutableByteArray#" %s_44} @ t_937 -> (T_Int64) @ t_938 -> {"State#" %s_44} @ t_939 -> {"ghc-prim_GHC.Prim.Unit#" (T_Double) @ t_940} @ t_941
    "readStablePtrArray#"         :: {"MutableByteArray#" %s_45} @ t_942 -> (T_Int64) @ t_943 -> {"State#" %s_45} @ t_944 -> {"ghc-prim_GHC.Prim.Unit#" {"StablePtr#" %a_36} @ t_945} @ t_946
    "readInt8Array#"              :: {"MutableByteArray#" %s_46} @ t_947 -> (T_Int64) @ t_948 -> {"State#" %s_46} @ t_949 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_950} @ t_951
    "readInt16Array#"             :: {"MutableByteArray#" %s_47} @ t_952 -> (T_Int64) @ t_953 -> {"State#" %s_47} @ t_954 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_955} @ t_956
    "readInt32Array#"             :: {"MutableByteArray#" %s_48} @ t_957 -> (T_Int64) @ t_958 -> {"State#" %s_48} @ t_959 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_960} @ t_961
    "readInt64Array#"             :: {"MutableByteArray#" %s_49} @ t_962 -> (T_Int64) @ t_963 -> {"State#" %s_49} @ t_964 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_965} @ t_966
    "readWord8Array#"             :: {"MutableByteArray#" %s_50} @ t_967 -> (T_Int64) @ t_968 -> {"State#" %s_50} @ t_969 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_970} @ t_971
    "readWord16Array#"            :: {"MutableByteArray#" %s_51} @ t_972 -> (T_Int64) @ t_973 -> {"State#" %s_51} @ t_974 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_975} @ t_976
    "readWord32Array#"            :: {"MutableByteArray#" %s_52} @ t_977 -> (T_Int64) @ t_978 -> {"State#" %s_52} @ t_979 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_980} @ t_981
    "readWord64Array#"            :: {"MutableByteArray#" %s_53} @ t_982 -> (T_Int64) @ t_983 -> {"State#" %s_53} @ t_984 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_985} @ t_986
    "readWord8ArrayAsChar#"       :: {"MutableByteArray#" %s_54} @ t_987 -> (T_Int64) @ t_988 -> {"State#" %s_54} @ t_989 -> {"ghc-prim_GHC.Prim.Unit#" (T_Char) @ t_990} @ t_991
    "readWord8ArrayAsWideChar#"   :: {"MutableByteArray#" %s_55} @ t_992 -> (T_Int64) @ t_993 -> {"State#" %s_55} @ t_994 -> {"ghc-prim_GHC.Prim.Unit#" (T_Char) @ t_995} @ t_996
    "readWord8ArrayAsAddr#"       :: {"MutableByteArray#" %s_56} @ t_997 -> (T_Int64) @ t_998 -> {"State#" %s_56} @ t_999 -> {"ghc-prim_GHC.Prim.Unit#" (T_Addr) @ t_1000} @ t_1001
    "readWord8ArrayAsFloat#"      :: {"MutableByteArray#" %s_57} @ t_1002 -> (T_Int64) @ t_1003 -> {"State#" %s_57} @ t_1004 -> {"ghc-prim_GHC.Prim.Unit#" (T_Float) @ t_1005} @ t_1006
    "readWord8ArrayAsDouble#"     :: {"MutableByteArray#" %s_58} @ t_1007 -> (T_Int64) @ t_1008 -> {"State#" %s_58} @ t_1009 -> {"ghc-prim_GHC.Prim.Unit#" (T_Double) @ t_1010} @ t_1011
    "readWord8ArrayAsStablePtr#"  :: {"MutableByteArray#" %s_59} @ t_1012 -> (T_Int64) @ t_1013 -> {"State#" %s_59} @ t_1014 -> {"ghc-prim_GHC.Prim.Unit#" {"StablePtr#" %a_37} @ t_1015} @ t_1016
    "readWord8ArrayAsInt16#"      :: {"MutableByteArray#" %s_60} @ t_1017 -> (T_Int64) @ t_1018 -> {"State#" %s_60} @ t_1019 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1020} @ t_1021
    "readWord8ArrayAsInt32#"      :: {"MutableByteArray#" %s_61} @ t_1022 -> (T_Int64) @ t_1023 -> {"State#" %s_61} @ t_1024 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1025} @ t_1026
    "readWord8ArrayAsInt64#"      :: {"MutableByteArray#" %s_62} @ t_1027 -> (T_Int64) @ t_1028 -> {"State#" %s_62} @ t_1029 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1030} @ t_1031
    "readWord8ArrayAsInt#"        :: {"MutableByteArray#" %s_63} @ t_1032 -> (T_Int64) @ t_1033 -> {"State#" %s_63} @ t_1034 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1035} @ t_1036
    "readWord8ArrayAsWord16#"     :: {"MutableByteArray#" %s_64} @ t_1037 -> (T_Int64) @ t_1038 -> {"State#" %s_64} @ t_1039 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1040} @ t_1041
    "readWord8ArrayAsWord32#"     :: {"MutableByteArray#" %s_65} @ t_1042 -> (T_Int64) @ t_1043 -> {"State#" %s_65} @ t_1044 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1045} @ t_1046
    "readWord8ArrayAsWord64#"     :: {"MutableByteArray#" %s_66} @ t_1047 -> (T_Int64) @ t_1048 -> {"State#" %s_66} @ t_1049 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1050} @ t_1051
    "readWord8ArrayAsWord#"       :: {"MutableByteArray#" %s_67} @ t_1052 -> (T_Int64) @ t_1053 -> {"State#" %s_67} @ t_1054 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1055} @ t_1056
    "writeCharArray#"             :: {"MutableByteArray#" %s_68} @ t_1057 -> (T_Int64) @ t_1058 -> (T_Char) @ t_1059 -> {"State#" %s_68} @ t_1060 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1061
    "writeWideCharArray#"         :: {"MutableByteArray#" %s_69} @ t_1062 -> (T_Int64) @ t_1063 -> (T_Char) @ t_1064 -> {"State#" %s_69} @ t_1065 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1066
    "writeIntArray#"              :: {"MutableByteArray#" %s_70} @ t_1067 -> (T_Int64) @ t_1068 -> (T_Int64) @ t_1069 -> {"State#" %s_70} @ t_1070 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1071
    "writeWordArray#"             :: {"MutableByteArray#" %s_71} @ t_1072 -> (T_Int64) @ t_1073 -> (T_Word64) @ t_1074 -> {"State#" %s_71} @ t_1075 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1076
    "writeAddrArray#"             :: {"MutableByteArray#" %s_72} @ t_1077 -> (T_Int64) @ t_1078 -> (T_Addr) @ t_1079 -> {"State#" %s_72} @ t_1080 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1081
    "writeFloatArray#"            :: {"MutableByteArray#" %s_73} @ t_1082 -> (T_Int64) @ t_1083 -> (T_Float) @ t_1084 -> {"State#" %s_73} @ t_1085 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1086
    "writeDoubleArray#"           :: {"MutableByteArray#" %s_74} @ t_1087 -> (T_Int64) @ t_1088 -> (T_Double) @ t_1089 -> {"State#" %s_74} @ t_1090 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1091
    "writeStablePtrArray#"        :: {"MutableByteArray#" %s_75} @ t_1092 -> (T_Int64) @ t_1093 -> {"StablePtr#" %a_38} @ t_1094 -> {"State#" %s_75} @ t_1095 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1096
    "writeInt8Array#"             :: {"MutableByteArray#" %s_76} @ t_1097 -> (T_Int64) @ t_1098 -> (T_Int64) @ t_1099 -> {"State#" %s_76} @ t_1100 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1101
    "writeInt16Array#"            :: {"MutableByteArray#" %s_77} @ t_1102 -> (T_Int64) @ t_1103 -> (T_Int64) @ t_1104 -> {"State#" %s_77} @ t_1105 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1106
    "writeInt32Array#"            :: {"MutableByteArray#" %s_78} @ t_1107 -> (T_Int64) @ t_1108 -> (T_Int64) @ t_1109 -> {"State#" %s_78} @ t_1110 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1111
    "writeInt64Array#"            :: {"MutableByteArray#" %s_79} @ t_1112 -> (T_Int64) @ t_1113 -> (T_Int64) @ t_1114 -> {"State#" %s_79} @ t_1115 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1116
    "writeWord8Array#"            :: {"MutableByteArray#" %s_80} @ t_1117 -> (T_Int64) @ t_1118 -> (T_Word64) @ t_1119 -> {"State#" %s_80} @ t_1120 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1121
    "writeWord16Array#"           :: {"MutableByteArray#" %s_81} @ t_1122 -> (T_Int64) @ t_1123 -> (T_Word64) @ t_1124 -> {"State#" %s_81} @ t_1125 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1126
    "writeWord32Array#"           :: {"MutableByteArray#" %s_82} @ t_1127 -> (T_Int64) @ t_1128 -> (T_Word64) @ t_1129 -> {"State#" %s_82} @ t_1130 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1131
    "writeWord64Array#"           :: {"MutableByteArray#" %s_83} @ t_1132 -> (T_Int64) @ t_1133 -> (T_Word64) @ t_1134 -> {"State#" %s_83} @ t_1135 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1136
    "writeWord8ArrayAsChar#"      :: {"MutableByteArray#" %s_84} @ t_1137 -> (T_Int64) @ t_1138 -> (T_Char) @ t_1139 -> {"State#" %s_84} @ t_1140 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1141
    "writeWord8ArrayAsWideChar#"  :: {"MutableByteArray#" %s_85} @ t_1142 -> (T_Int64) @ t_1143 -> (T_Char) @ t_1144 -> {"State#" %s_85} @ t_1145 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1146
    "writeWord8ArrayAsAddr#"      :: {"MutableByteArray#" %s_86} @ t_1147 -> (T_Int64) @ t_1148 -> (T_Addr) @ t_1149 -> {"State#" %s_86} @ t_1150 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1151
    "writeWord8ArrayAsFloat#"     :: {"MutableByteArray#" %s_87} @ t_1152 -> (T_Int64) @ t_1153 -> (T_Float) @ t_1154 -> {"State#" %s_87} @ t_1155 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1156
    "writeWord8ArrayAsDouble#"    :: {"MutableByteArray#" %s_88} @ t_1157 -> (T_Int64) @ t_1158 -> (T_Double) @ t_1159 -> {"State#" %s_88} @ t_1160 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1161
    "writeWord8ArrayAsStablePtr#" :: {"MutableByteArray#" %s_89} @ t_1162 -> (T_Int64) @ t_1163 -> {"StablePtr#" %a_39} @ t_1164 -> {"State#" %s_89} @ t_1165 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1166
    "writeWord8ArrayAsInt16#"     :: {"MutableByteArray#" %s_90} @ t_1167 -> (T_Int64) @ t_1168 -> (T_Int64) @ t_1169 -> {"State#" %s_90} @ t_1170 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1171
    "writeWord8ArrayAsInt32#"     :: {"MutableByteArray#" %s_91} @ t_1172 -> (T_Int64) @ t_1173 -> (T_Int64) @ t_1174 -> {"State#" %s_91} @ t_1175 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1176
    "writeWord8ArrayAsInt64#"     :: {"MutableByteArray#" %s_92} @ t_1177 -> (T_Int64) @ t_1178 -> (T_Int64) @ t_1179 -> {"State#" %s_92} @ t_1180 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1181
    "writeWord8ArrayAsInt#"       :: {"MutableByteArray#" %s_93} @ t_1182 -> (T_Int64) @ t_1183 -> (T_Int64) @ t_1184 -> {"State#" %s_93} @ t_1185 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1186
    "writeWord8ArrayAsWord16#"    :: {"MutableByteArray#" %s_94} @ t_1187 -> (T_Int64) @ t_1188 -> (T_Word64) @ t_1189 -> {"State#" %s_94} @ t_1190 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1191
    "writeWord8ArrayAsWord32#"    :: {"MutableByteArray#" %s_95} @ t_1192 -> (T_Int64) @ t_1193 -> (T_Word64) @ t_1194 -> {"State#" %s_95} @ t_1195 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1196
    "writeWord8ArrayAsWord64#"    :: {"MutableByteArray#" %s_96} @ t_1197 -> (T_Int64) @ t_1198 -> (T_Word64) @ t_1199 -> {"State#" %s_96} @ t_1200 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1201
    "writeWord8ArrayAsWord#"      :: {"MutableByteArray#" %s_97} @ t_1202 -> (T_Int64) @ t_1203 -> (T_Word64) @ t_1204 -> {"State#" %s_97} @ t_1205 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1206

  primop pure
    "compareByteArrays#" :: {"ByteArray#"} @ t_1207 -> (T_Int64) @ t_1208 -> {"ByteArray#"} @ t_1209 -> (T_Int64) @ t_1210 -> (T_Int64) @ t_1211 -> (T_Int64) @ t_1212

  primop effectful
    "copyByteArray#"              :: {"ByteArray#"} @ t_1213 -> (T_Int64) @ t_1214 -> {"MutableByteArray#" %s_98} @ t_1215 -> (T_Int64) @ t_1216 -> (T_Int64) @ t_1217 -> {"State#" %s_98} @ t_1218 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1219
    "copyMutableByteArray#"       :: {"MutableByteArray#" %s_99} @ t_1220 -> (T_Int64) @ t_1221 -> {"MutableByteArray#" %s_99} @ t_1222 -> (T_Int64) @ t_1223 -> (T_Int64) @ t_1224 -> {"State#" %s_99} @ t_1225 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1226
    "copyByteArrayToAddr#"        :: {"ByteArray#"} @ t_1227 -> (T_Int64) @ t_1228 -> (T_Addr) @ t_1229 -> (T_Int64) @ t_1230 -> {"State#" %s_100} @ t_1231 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1232
    "copyMutableByteArrayToAddr#" :: {"MutableByteArray#" %s_101} @ t_1233 -> (T_Int64) @ t_1234 -> (T_Addr) @ t_1235 -> (T_Int64) @ t_1236 -> {"State#" %s_101} @ t_1237 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1238
    "copyAddrToByteArray#"        :: (T_Addr) @ t_1239 -> {"MutableByteArray#" %s_102} @ t_1240 -> (T_Int64) @ t_1241 -> (T_Int64) @ t_1242 -> {"State#" %s_102} @ t_1243 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1244
    "setByteArray#"               :: {"MutableByteArray#" %s_103} @ t_1245 -> (T_Int64) @ t_1246 -> (T_Int64) @ t_1247 -> (T_Int64) @ t_1248 -> {"State#" %s_103} @ t_1249 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1250
    "atomicReadIntArray#"         :: {"MutableByteArray#" %s_104} @ t_1251 -> (T_Int64) @ t_1252 -> {"State#" %s_104} @ t_1253 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1254} @ t_1255
    "atomicWriteIntArray#"        :: {"MutableByteArray#" %s_105} @ t_1256 -> (T_Int64) @ t_1257 -> (T_Int64) @ t_1258 -> {"State#" %s_105} @ t_1259 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1260
    "casIntArray#"                :: {"MutableByteArray#" %s_106} @ t_1261 -> (T_Int64) @ t_1262 -> (T_Int64) @ t_1263 -> (T_Int64) @ t_1264 -> {"State#" %s_106} @ t_1265 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1266} @ t_1267
    "fetchAddIntArray#"           :: {"MutableByteArray#" %s_107} @ t_1268 -> (T_Int64) @ t_1269 -> (T_Int64) @ t_1270 -> {"State#" %s_107} @ t_1271 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1272} @ t_1273
    "fetchSubIntArray#"           :: {"MutableByteArray#" %s_108} @ t_1274 -> (T_Int64) @ t_1275 -> (T_Int64) @ t_1276 -> {"State#" %s_108} @ t_1277 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1278} @ t_1279
    "fetchAndIntArray#"           :: {"MutableByteArray#" %s_109} @ t_1280 -> (T_Int64) @ t_1281 -> (T_Int64) @ t_1282 -> {"State#" %s_109} @ t_1283 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1284} @ t_1285
    "fetchNandIntArray#"          :: {"MutableByteArray#" %s_110} @ t_1286 -> (T_Int64) @ t_1287 -> (T_Int64) @ t_1288 -> {"State#" %s_110} @ t_1289 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1290} @ t_1291
    "fetchOrIntArray#"            :: {"MutableByteArray#" %s_111} @ t_1292 -> (T_Int64) @ t_1293 -> (T_Int64) @ t_1294 -> {"State#" %s_111} @ t_1295 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1296} @ t_1297
    "fetchXorIntArray#"           :: {"MutableByteArray#" %s_112} @ t_1298 -> (T_Int64) @ t_1299 -> (T_Int64) @ t_1300 -> {"State#" %s_112} @ t_1301 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1302} @ t_1303

  {-
    Arrays of arrays
  -}
  primop effectful
    "newArrayArray#" :: (T_Int64) @ t_1304 -> {"State#" %s_113} @ t_1305 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableArrayArray#" %s_113} @ t_1306} @ t_1307

  primop pure
    "sameMutableArrayArray#" :: {"MutableArrayArray#" %s_114} @ t_1308 -> {"MutableArrayArray#" %s_114} @ t_1309 -> (T_Int64) @ t_1310

  primop effectful
    "unsafeFreezeArrayArray#" :: {"MutableArrayArray#" %s_115} @ t_1311 -> {"State#" %s_115} @ t_1312 -> {"ghc-prim_GHC.Prim.Unit#" {"ArrayArray#"} @ t_1313} @ t_1314

  primop pure
    "sizeofArrayArray#"        :: {"ArrayArray#"} @ t_1315 -> (T_Int64) @ t_1316
    "sizeofMutableArrayArray#" :: {"MutableArrayArray#" %s_116} @ t_1317 -> (T_Int64) @ t_1318
    "indexByteArrayArray#"     :: {"ArrayArray#"} @ t_1319 -> (T_Int64) @ t_1320 -> {"ByteArray#"} @ t_1321
    "indexArrayArrayArray#"    :: {"ArrayArray#"} @ t_1322 -> (T_Int64) @ t_1323 -> {"ArrayArray#"} @ t_1324

  primop effectful
    "readByteArrayArray#"          :: {"MutableArrayArray#" %s_117} @ t_1325 -> (T_Int64) @ t_1326 -> {"State#" %s_117} @ t_1327 -> {"ghc-prim_GHC.Prim.Unit#" {"ByteArray#"} @ t_1328} @ t_1329
    "readMutableByteArrayArray#"   :: {"MutableArrayArray#" %s_118} @ t_1330 -> (T_Int64) @ t_1331 -> {"State#" %s_118} @ t_1332 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableByteArray#" %s_118} @ t_1333} @ t_1334
    "readArrayArrayArray#"         :: {"MutableArrayArray#" %s_119} @ t_1335 -> (T_Int64) @ t_1336 -> {"State#" %s_119} @ t_1337 -> {"ghc-prim_GHC.Prim.Unit#" {"ArrayArray#"} @ t_1338} @ t_1339
    "readMutableArrayArrayArray#"  :: {"MutableArrayArray#" %s_120} @ t_1340 -> (T_Int64) @ t_1341 -> {"State#" %s_120} @ t_1342 -> {"ghc-prim_GHC.Prim.Unit#" {"MutableArrayArray#" %s_120} @ t_1343} @ t_1344
    "writeByteArrayArray#"         :: {"MutableArrayArray#" %s_121} @ t_1345 -> (T_Int64) @ t_1346 -> {"ByteArray#"} @ t_1347 -> {"State#" %s_121} @ t_1348 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1349
    "writeMutableByteArrayArray#"  :: {"MutableArrayArray#" %s_122} @ t_1350 -> (T_Int64) @ t_1351 -> {"MutableByteArray#" %s_122} @ t_1352 -> {"State#" %s_122} @ t_1353 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1354
    "writeArrayArrayArray#"        :: {"MutableArrayArray#" %s_123} @ t_1355 -> (T_Int64) @ t_1356 -> {"ArrayArray#"} @ t_1357 -> {"State#" %s_123} @ t_1358 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1359
    "writeMutableArrayArrayArray#" :: {"MutableArrayArray#" %s_124} @ t_1360 -> (T_Int64) @ t_1361 -> {"MutableArrayArray#" %s_124} @ t_1362 -> {"State#" %s_124} @ t_1363 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1364
    "copyArrayArray#"              :: {"ArrayArray#"} @ t_1365 -> (T_Int64) @ t_1366 -> {"MutableArrayArray#" %s_125} @ t_1367 -> (T_Int64) @ t_1368 -> (T_Int64) @ t_1369 -> {"State#" %s_125} @ t_1370 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1371
    "copyMutableArrayArray#"       :: {"MutableArrayArray#" %s_126} @ t_1372 -> (T_Int64) @ t_1373 -> {"MutableArrayArray#" %s_126} @ t_1374 -> (T_Int64) @ t_1375 -> (T_Int64) @ t_1376 -> {"State#" %s_126} @ t_1377 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1378

  {-
    Addr#
  -}
  primop pure
    "plusAddr#"              :: (T_Addr) @ t_1379 -> (T_Int64) @ t_1380 -> (T_Addr) @ t_1381
    "minusAddr#"             :: (T_Addr) @ t_1382 -> (T_Addr) @ t_1383 -> (T_Int64) @ t_1384
    "remAddr#"               :: (T_Addr) @ t_1385 -> (T_Int64) @ t_1386 -> (T_Int64) @ t_1387
    "addr2Int#"              :: (T_Addr) @ t_1388 -> (T_Int64) @ t_1389
    "int2Addr#"              :: (T_Int64) @ t_1390 -> (T_Addr) @ t_1391
    "gtAddr#"                :: (T_Addr) @ t_1392 -> (T_Addr) @ t_1393 -> (T_Int64) @ t_1394
    "geAddr#"                :: (T_Addr) @ t_1395 -> (T_Addr) @ t_1396 -> (T_Int64) @ t_1397
    "eqAddr#"                :: (T_Addr) @ t_1398 -> (T_Addr) @ t_1399 -> (T_Int64) @ t_1400
    "neAddr#"                :: (T_Addr) @ t_1401 -> (T_Addr) @ t_1402 -> (T_Int64) @ t_1403
    "ltAddr#"                :: (T_Addr) @ t_1404 -> (T_Addr) @ t_1405 -> (T_Int64) @ t_1406
    "leAddr#"                :: (T_Addr) @ t_1407 -> (T_Addr) @ t_1408 -> (T_Int64) @ t_1409
    "indexCharOffAddr#"      :: (T_Addr) @ t_1410 -> (T_Int64) @ t_1411 -> (T_Char) @ t_1412
    "indexWideCharOffAddr#"  :: (T_Addr) @ t_1413 -> (T_Int64) @ t_1414 -> (T_Char) @ t_1415
    "indexIntOffAddr#"       :: (T_Addr) @ t_1416 -> (T_Int64) @ t_1417 -> (T_Int64) @ t_1418
    "indexWordOffAddr#"      :: (T_Addr) @ t_1419 -> (T_Int64) @ t_1420 -> (T_Word64) @ t_1421
    "indexAddrOffAddr#"      :: (T_Addr) @ t_1422 -> (T_Int64) @ t_1423 -> (T_Addr) @ t_1424
    "indexFloatOffAddr#"     :: (T_Addr) @ t_1425 -> (T_Int64) @ t_1426 -> (T_Float) @ t_1427
    "indexDoubleOffAddr#"    :: (T_Addr) @ t_1428 -> (T_Int64) @ t_1429 -> (T_Double) @ t_1430
    "indexStablePtrOffAddr#" :: (T_Addr) @ t_1431 -> (T_Int64) @ t_1432 -> {"StablePtr#" %a_40} @ t_1433
    "indexInt8OffAddr#"      :: (T_Addr) @ t_1434 -> (T_Int64) @ t_1435 -> (T_Int64) @ t_1436
    "indexInt16OffAddr#"     :: (T_Addr) @ t_1437 -> (T_Int64) @ t_1438 -> (T_Int64) @ t_1439
    "indexInt32OffAddr#"     :: (T_Addr) @ t_1440 -> (T_Int64) @ t_1441 -> (T_Int64) @ t_1442
    "indexInt64OffAddr#"     :: (T_Addr) @ t_1443 -> (T_Int64) @ t_1444 -> (T_Int64) @ t_1445
    "indexWord8OffAddr#"     :: (T_Addr) @ t_1446 -> (T_Int64) @ t_1447 -> (T_Word64) @ t_1448
    "indexWord16OffAddr#"    :: (T_Addr) @ t_1449 -> (T_Int64) @ t_1450 -> (T_Word64) @ t_1451
    "indexWord32OffAddr#"    :: (T_Addr) @ t_1452 -> (T_Int64) @ t_1453 -> (T_Word64) @ t_1454
    "indexWord64OffAddr#"    :: (T_Addr) @ t_1455 -> (T_Int64) @ t_1456 -> (T_Word64) @ t_1457

  primop effectful
    "readCharOffAddr#"       :: (T_Addr) @ t_1458 -> (T_Int64) @ t_1459 -> {"State#" %s_127} @ t_1460 -> {"ghc-prim_GHC.Prim.Unit#" (T_Char) @ t_1461} @ t_1462
    "readWideCharOffAddr#"   :: (T_Addr) @ t_1463 -> (T_Int64) @ t_1464 -> {"State#" %s_128} @ t_1465 -> {"ghc-prim_GHC.Prim.Unit#" (T_Char) @ t_1466} @ t_1467
    "readIntOffAddr#"        :: (T_Addr) @ t_1468 -> (T_Int64) @ t_1469 -> {"State#" %s_129} @ t_1470 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1471} @ t_1472
    "readWordOffAddr#"       :: (T_Addr) @ t_1473 -> (T_Int64) @ t_1474 -> {"State#" %s_130} @ t_1475 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1476} @ t_1477
    "readAddrOffAddr#"       :: (T_Addr) @ t_1478 -> (T_Int64) @ t_1479 -> {"State#" %s_131} @ t_1480 -> {"ghc-prim_GHC.Prim.Unit#" (T_Addr) @ t_1481} @ t_1482
    "readFloatOffAddr#"      :: (T_Addr) @ t_1483 -> (T_Int64) @ t_1484 -> {"State#" %s_132} @ t_1485 -> {"ghc-prim_GHC.Prim.Unit#" (T_Float) @ t_1486} @ t_1487
    "readDoubleOffAddr#"     :: (T_Addr) @ t_1488 -> (T_Int64) @ t_1489 -> {"State#" %s_133} @ t_1490 -> {"ghc-prim_GHC.Prim.Unit#" (T_Double) @ t_1491} @ t_1492
    "readStablePtrOffAddr#"  :: (T_Addr) @ t_1493 -> (T_Int64) @ t_1494 -> {"State#" %s_134} @ t_1495 -> {"ghc-prim_GHC.Prim.Unit#" {"StablePtr#" %a_41} @ t_1496} @ t_1497
    "readInt8OffAddr#"       :: (T_Addr) @ t_1498 -> (T_Int64) @ t_1499 -> {"State#" %s_135} @ t_1500 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1501} @ t_1502
    "readInt16OffAddr#"      :: (T_Addr) @ t_1503 -> (T_Int64) @ t_1504 -> {"State#" %s_136} @ t_1505 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1506} @ t_1507
    "readInt32OffAddr#"      :: (T_Addr) @ t_1508 -> (T_Int64) @ t_1509 -> {"State#" %s_137} @ t_1510 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1511} @ t_1512
    "readInt64OffAddr#"      :: (T_Addr) @ t_1513 -> (T_Int64) @ t_1514 -> {"State#" %s_138} @ t_1515 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1516} @ t_1517
    "readWord8OffAddr#"      :: (T_Addr) @ t_1518 -> (T_Int64) @ t_1519 -> {"State#" %s_139} @ t_1520 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1521} @ t_1522
    "readWord16OffAddr#"     :: (T_Addr) @ t_1523 -> (T_Int64) @ t_1524 -> {"State#" %s_140} @ t_1525 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1526} @ t_1527
    "readWord32OffAddr#"     :: (T_Addr) @ t_1528 -> (T_Int64) @ t_1529 -> {"State#" %s_141} @ t_1530 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1531} @ t_1532
    "readWord64OffAddr#"     :: (T_Addr) @ t_1533 -> (T_Int64) @ t_1534 -> {"State#" %s_142} @ t_1535 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1536} @ t_1537
    "writeCharOffAddr#"      :: (T_Addr) @ t_1538 -> (T_Int64) @ t_1539 -> (T_Char) @ t_1540 -> {"State#" %s_143} @ t_1541 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1542
    "writeWideCharOffAddr#"  :: (T_Addr) @ t_1543 -> (T_Int64) @ t_1544 -> (T_Char) @ t_1545 -> {"State#" %s_144} @ t_1546 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1547
    "writeIntOffAddr#"       :: (T_Addr) @ t_1548 -> (T_Int64) @ t_1549 -> (T_Int64) @ t_1550 -> {"State#" %s_145} @ t_1551 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1552
    "writeWordOffAddr#"      :: (T_Addr) @ t_1553 -> (T_Int64) @ t_1554 -> (T_Word64) @ t_1555 -> {"State#" %s_146} @ t_1556 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1557
    "writeAddrOffAddr#"      :: (T_Addr) @ t_1558 -> (T_Int64) @ t_1559 -> (T_Addr) @ t_1560 -> {"State#" %s_147} @ t_1561 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1562
    "writeFloatOffAddr#"     :: (T_Addr) @ t_1563 -> (T_Int64) @ t_1564 -> (T_Float) @ t_1565 -> {"State#" %s_148} @ t_1566 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1567
    "writeDoubleOffAddr#"    :: (T_Addr) @ t_1568 -> (T_Int64) @ t_1569 -> (T_Double) @ t_1570 -> {"State#" %s_149} @ t_1571 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1572
    "writeStablePtrOffAddr#" :: (T_Addr) @ t_1573 -> (T_Int64) @ t_1574 -> {"StablePtr#" %a_42} @ t_1575 -> {"State#" %s_150} @ t_1576 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1577
    "writeInt8OffAddr#"      :: (T_Addr) @ t_1578 -> (T_Int64) @ t_1579 -> (T_Int64) @ t_1580 -> {"State#" %s_151} @ t_1581 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1582
    "writeInt16OffAddr#"     :: (T_Addr) @ t_1583 -> (T_Int64) @ t_1584 -> (T_Int64) @ t_1585 -> {"State#" %s_152} @ t_1586 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1587
    "writeInt32OffAddr#"     :: (T_Addr) @ t_1588 -> (T_Int64) @ t_1589 -> (T_Int64) @ t_1590 -> {"State#" %s_153} @ t_1591 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1592
    "writeInt64OffAddr#"     :: (T_Addr) @ t_1593 -> (T_Int64) @ t_1594 -> (T_Int64) @ t_1595 -> {"State#" %s_154} @ t_1596 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1597
    "writeWord8OffAddr#"     :: (T_Addr) @ t_1598 -> (T_Int64) @ t_1599 -> (T_Word64) @ t_1600 -> {"State#" %s_155} @ t_1601 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1602
    "writeWord16OffAddr#"    :: (T_Addr) @ t_1603 -> (T_Int64) @ t_1604 -> (T_Word64) @ t_1605 -> {"State#" %s_156} @ t_1606 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1607
    "writeWord32OffAddr#"    :: (T_Addr) @ t_1608 -> (T_Int64) @ t_1609 -> (T_Word64) @ t_1610 -> {"State#" %s_157} @ t_1611 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1612
    "writeWord64OffAddr#"    :: (T_Addr) @ t_1613 -> (T_Int64) @ t_1614 -> (T_Word64) @ t_1615 -> {"State#" %s_158} @ t_1616 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1617

  {-
    Mutable variables
  -}
  primop effectful
    "newMutVar#"   :: %a_43 -> {"State#" %s_159} @ t_1618 -> {"ghc-prim_GHC.Prim.Unit#" {"MutVar#" %s_159 %a_43} @ t_1619} @ t_1620
    "readMutVar#"  :: {"MutVar#" %s_160 %a_44} @ t_1621 -> {"State#" %s_160} @ t_1622 -> {"ghc-prim_GHC.Prim.Unit#" %a_44} @ t_1623
    "writeMutVar#" :: {"MutVar#" %s_161 %a_45} @ t_1624 -> %a_45 -> {"State#" %s_161} @ t_1625 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1626

  primop pure
    "sameMutVar#" :: {"MutVar#" %s_162 %a_46} @ t_1627 -> {"MutVar#" %s_162 %a_46} @ t_1628 -> (T_Int64) @ t_1629

  primop effectful
    "atomicModifyMutVar2#" :: {"MutVar#" %s_163 %a_47} @ t_1630 -> (tf_1060 : %a_47 -> %c_0) -> {"State#" %s_163} @ t_1631 -> {"ghc-prim_GHC.Prim.(#,#)" %a_47 %c_0} @ t_1632
    "atomicModifyMutVar_#" :: {"MutVar#" %s_164 %a_48} @ t_1633 -> (tf_1064 : %a_48 -> %a_48) -> {"State#" %s_164} @ t_1634 -> {"ghc-prim_GHC.Prim.(#,#)" %a_48 %a_48} @ t_1635
    "casMutVar#"           :: {"MutVar#" %s_165 %a_49} @ t_1636 -> %a_49 -> %a_49 -> {"State#" %s_165} @ t_1637 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_1638 %a_49} @ t_1639

  {-
    Exceptions
  -}
  primop effectful
    "catch#"                 :: (tf_1072 : {"State#" {RealWorld} @ t_1640} @ t_1641 -> {"ghc-prim_GHC.Prim.Unit#" %a_50} @ t_1642) -> (tf_1074 : %b_0 -> {"State#" {RealWorld} @ t_1643} @ t_1644 -> {"ghc-prim_GHC.Prim.Unit#" %a_50} @ t_1645) -> {"State#" {RealWorld} @ t_1646} @ t_1647 -> {"ghc-prim_GHC.Prim.Unit#" %a_50} @ t_1648
    "raise#"                 :: %b_1 -> %o_0
    "raiseDivZero#"          :: {"Void#"} @ t_1649 -> %o_1
    "raiseUnderflow#"        :: {"Void#"} @ t_1650 -> %o_2
    "raiseOverflow#"         :: {"Void#"} @ t_1651 -> %o_3
    "raiseIO#"               :: %a_51 -> {"State#" {RealWorld} @ t_1652} @ t_1653 -> {"ghc-prim_GHC.Prim.Unit#" %b_2} @ t_1654
    "maskAsyncExceptions#"   :: (tf_1084 : {"State#" {RealWorld} @ t_1655} @ t_1656 -> {"ghc-prim_GHC.Prim.Unit#" %a_52} @ t_1657) -> {"State#" {RealWorld} @ t_1658} @ t_1659 -> {"ghc-prim_GHC.Prim.Unit#" %a_52} @ t_1660
    "maskUninterruptible#"   :: (tf_1087 : {"State#" {RealWorld} @ t_1661} @ t_1662 -> {"ghc-prim_GHC.Prim.Unit#" %a_53} @ t_1663) -> {"State#" {RealWorld} @ t_1664} @ t_1665 -> {"ghc-prim_GHC.Prim.Unit#" %a_53} @ t_1666
    "unmaskAsyncExceptions#" :: (tf_1090 : {"State#" {RealWorld} @ t_1667} @ t_1668 -> {"ghc-prim_GHC.Prim.Unit#" %a_54} @ t_1669) -> {"State#" {RealWorld} @ t_1670} @ t_1671 -> {"ghc-prim_GHC.Prim.Unit#" %a_54} @ t_1672
    "getMaskingState#"       :: {"State#" {RealWorld} @ t_1673} @ t_1674 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1675} @ t_1676

  {-
    STM-accessible Mutable Variables
  -}
  primop effectful
    "atomically#" :: (tf_1094 : {"State#" {RealWorld} @ t_1677} @ t_1678 -> {"ghc-prim_GHC.Prim.Unit#" %a_55} @ t_1679) -> {"State#" {RealWorld} @ t_1680} @ t_1681 -> {"ghc-prim_GHC.Prim.Unit#" %a_55} @ t_1682
    "retry#"      :: {"State#" {RealWorld} @ t_1683} @ t_1684 -> {"ghc-prim_GHC.Prim.Unit#" %a_56} @ t_1685
    "catchRetry#" :: (tf_1098 : {"State#" {RealWorld} @ t_1686} @ t_1687 -> {"ghc-prim_GHC.Prim.Unit#" %a_57} @ t_1688) -> (tf_1099 : {"State#" {RealWorld} @ t_1689} @ t_1690 -> {"ghc-prim_GHC.Prim.Unit#" %a_57} @ t_1691) -> {"State#" {RealWorld} @ t_1692} @ t_1693 -> {"ghc-prim_GHC.Prim.Unit#" %a_57} @ t_1694
    "catchSTM#"   :: (tf_1103 : {"State#" {RealWorld} @ t_1695} @ t_1696 -> {"ghc-prim_GHC.Prim.Unit#" %a_58} @ t_1697) -> (tf_1105 : %b_3 -> {"State#" {RealWorld} @ t_1698} @ t_1699 -> {"ghc-prim_GHC.Prim.Unit#" %a_58} @ t_1700) -> {"State#" {RealWorld} @ t_1701} @ t_1702 -> {"ghc-prim_GHC.Prim.Unit#" %a_58} @ t_1703
    "newTVar#"    :: %a_59 -> {"State#" %s_166} @ t_1704 -> {"ghc-prim_GHC.Prim.Unit#" {"TVar#" %s_166 %a_59} @ t_1705} @ t_1706
    "readTVar#"   :: {"TVar#" %s_167 %a_60} @ t_1707 -> {"State#" %s_167} @ t_1708 -> {"ghc-prim_GHC.Prim.Unit#" %a_60} @ t_1709
    "readTVarIO#" :: {"TVar#" %s_168 %a_61} @ t_1710 -> {"State#" %s_168} @ t_1711 -> {"ghc-prim_GHC.Prim.Unit#" %a_61} @ t_1712
    "writeTVar#"  :: {"TVar#" %s_169 %a_62} @ t_1713 -> %a_62 -> {"State#" %s_169} @ t_1714 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1715

  primop pure
    "sameTVar#" :: {"TVar#" %s_170 %a_63} @ t_1716 -> {"TVar#" %s_170 %a_63} @ t_1717 -> (T_Int64) @ t_1718

  {-
    Synchronized Mutable Variables
  -}
  primop effectful
    "newMVar#"     :: {"State#" %s_171} @ t_1719 -> {"ghc-prim_GHC.Prim.Unit#" {"MVar#" %s_171 %a_64} @ t_1720} @ t_1721
    "takeMVar#"    :: {"MVar#" %s_172 %a_65} @ t_1722 -> {"State#" %s_172} @ t_1723 -> {"ghc-prim_GHC.Prim.Unit#" %a_65} @ t_1724
    "tryTakeMVar#" :: {"MVar#" %s_173 %a_66} @ t_1725 -> {"State#" %s_173} @ t_1726 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_1727 %a_66} @ t_1728
    "putMVar#"     :: {"MVar#" %s_174 %a_67} @ t_1729 -> %a_67 -> {"State#" %s_174} @ t_1730 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1731
    "tryPutMVar#"  :: {"MVar#" %s_175 %a_68} @ t_1732 -> %a_68 -> {"State#" %s_175} @ t_1733 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1734} @ t_1735
    "readMVar#"    :: {"MVar#" %s_176 %a_69} @ t_1736 -> {"State#" %s_176} @ t_1737 -> {"ghc-prim_GHC.Prim.Unit#" %a_69} @ t_1738
    "tryReadMVar#" :: {"MVar#" %s_177 %a_70} @ t_1739 -> {"State#" %s_177} @ t_1740 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_1741 %a_70} @ t_1742

  primop pure
    "sameMVar#" :: {"MVar#" %s_178 %a_71} @ t_1743 -> {"MVar#" %s_178 %a_71} @ t_1744 -> (T_Int64) @ t_1745

  primop effectful
    "isEmptyMVar#" :: {"MVar#" %s_179 %a_72} @ t_1746 -> {"State#" %s_179} @ t_1747 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1748} @ t_1749

  {-
    Delay/wait operations
  -}
  primop effectful
    "delay#"     :: (T_Int64) @ t_1750 -> {"State#" %s_180} @ t_1751 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1752
    "waitRead#"  :: (T_Int64) @ t_1753 -> {"State#" %s_181} @ t_1754 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1755
    "waitWrite#" :: (T_Int64) @ t_1756 -> {"State#" %s_182} @ t_1757 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1758

  {-
    Concurrency primitives
  -}
  primop effectful
    "fork#"                 :: %a_73 -> {"State#" {RealWorld} @ t_1759} @ t_1760 -> {"ghc-prim_GHC.Prim.Unit#" {"ThreadId#"} @ t_1761} @ t_1762
    "forkOn#"               :: (T_Int64) @ t_1763 -> %a_74 -> {"State#" {RealWorld} @ t_1764} @ t_1765 -> {"ghc-prim_GHC.Prim.Unit#" {"ThreadId#"} @ t_1766} @ t_1767
    "killThread#"           :: {"ThreadId#"} @ t_1768 -> %a_75 -> {"State#" {RealWorld} @ t_1769} @ t_1770 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1771
    "yield#"                :: {"State#" {RealWorld} @ t_1772} @ t_1773 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1774
    "myThreadId#"           :: {"State#" {RealWorld} @ t_1775} @ t_1776 -> {"ghc-prim_GHC.Prim.Unit#" {"ThreadId#"} @ t_1777} @ t_1778
    "labelThread#"          :: {"ThreadId#"} @ t_1779 -> (T_Addr) @ t_1780 -> {"State#" {RealWorld} @ t_1781} @ t_1782 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1783
    "isCurrentThreadBound#" :: {"State#" {RealWorld} @ t_1784} @ t_1785 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1786} @ t_1787
    "noDuplicate#"          :: {"State#" %s_183} @ t_1788 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1789
    "threadStatus#"         :: {"ThreadId#"} @ t_1790 -> {"State#" {RealWorld} @ t_1791} @ t_1792 -> {"ghc-prim_GHC.Prim.(#,,#)" (T_Int64) @ t_1793 (T_Int64) @ t_1794 (T_Int64) @ t_1795} @ t_1796

  {-
    Weak pointers
  -}
  primop effectful
    "mkWeak#"              :: %o_4 -> %b_4 -> (tf_1162 : {"State#" {RealWorld} @ t_1797} @ t_1798 -> {"ghc-prim_GHC.Prim.Unit#" %c_1} @ t_1799) -> {"State#" {RealWorld} @ t_1800} @ t_1801 -> {"ghc-prim_GHC.Prim.Unit#" {"Weak#" %b_4} @ t_1802} @ t_1803
    "mkWeakNoFinalizer#"   :: %o_5 -> %b_5 -> {"State#" {RealWorld} @ t_1804} @ t_1805 -> {"ghc-prim_GHC.Prim.Unit#" {"Weak#" %b_5} @ t_1806} @ t_1807
    "addCFinalizerToWeak#" :: (T_Addr) @ t_1808 -> (T_Addr) @ t_1809 -> (T_Int64) @ t_1810 -> (T_Addr) @ t_1811 -> {"Weak#" %b_6} @ t_1812 -> {"State#" {RealWorld} @ t_1813} @ t_1814 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1815} @ t_1816
    "deRefWeak#"           :: {"Weak#" %a_76} @ t_1817 -> {"State#" {RealWorld} @ t_1818} @ t_1819 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_1820 %a_76} @ t_1821
    "finalizeWeak#"        :: {"Weak#" %a_77} @ t_1822 -> {"State#" {RealWorld} @ t_1823} @ t_1824 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_1825 (tf_1178 : {"State#" {RealWorld} @ t_1826} @ t_1827 -> {"ghc-prim_GHC.Prim.Unit#" %b_7} @ t_1828)} @ t_1829
    "touch#"               :: %o_6 -> {"State#" {RealWorld} @ t_1830} @ t_1831 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1832

  {-
    Stable pointers and names
  -}
  primop effectful
    "makeStablePtr#"  :: %a_78 -> {"State#" {RealWorld} @ t_1833} @ t_1834 -> {"ghc-prim_GHC.Prim.Unit#" {"StablePtr#" %a_78} @ t_1835} @ t_1836
    "deRefStablePtr#" :: {"StablePtr#" %a_79} @ t_1837 -> {"State#" {RealWorld} @ t_1838} @ t_1839 -> {"ghc-prim_GHC.Prim.Unit#" %a_79} @ t_1840
    "eqStablePtr#"    :: {"StablePtr#" %a_80} @ t_1841 -> {"StablePtr#" %a_80} @ t_1842 -> (T_Int64) @ t_1843
    "makeStableName#" :: %a_81 -> {"State#" {RealWorld} @ t_1844} @ t_1845 -> {"ghc-prim_GHC.Prim.Unit#" {"StableName#" %a_81} @ t_1846} @ t_1847

  primop pure
    "eqStableName#"    :: {"StableName#" %a_82} @ t_1848 -> {"StableName#" %b_8} @ t_1849 -> (T_Int64) @ t_1850
    "stableNameToInt#" :: {"StableName#" %a_83} @ t_1851 -> (T_Int64) @ t_1852

  {-
    Compact normal form
  -}
  primop effectful
    "compactNew#"    :: (T_Word64) @ t_1853 -> {"State#" {RealWorld} @ t_1854} @ t_1855 -> {"ghc-prim_GHC.Prim.Unit#" {"Compact#"} @ t_1856} @ t_1857
    "compactResize#" :: {"Compact#"} @ t_1858 -> (T_Word64) @ t_1859 -> {"State#" {RealWorld} @ t_1860} @ t_1861 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1862

  primop pure
    "compactContains#"      :: {"Compact#"} @ t_1863 -> %a_84 -> {"State#" {RealWorld} @ t_1864} @ t_1865 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1866} @ t_1867
    "compactContainsAny#"   :: %a_85 -> {"State#" {RealWorld} @ t_1868} @ t_1869 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1870} @ t_1871
    "compactGetFirstBlock#" :: {"Compact#"} @ t_1872 -> {"State#" {RealWorld} @ t_1873} @ t_1874 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Addr) @ t_1875 (T_Word64) @ t_1876} @ t_1877
    "compactGetNextBlock#"  :: {"Compact#"} @ t_1878 -> (T_Addr) @ t_1879 -> {"State#" {RealWorld} @ t_1880} @ t_1881 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Addr) @ t_1882 (T_Word64) @ t_1883} @ t_1884

  primop effectful
    "compactAllocateBlock#"  :: (T_Word64) @ t_1885 -> (T_Addr) @ t_1886 -> {"State#" {RealWorld} @ t_1887} @ t_1888 -> {"ghc-prim_GHC.Prim.Unit#" (T_Addr) @ t_1889} @ t_1890
    "compactFixupPointers#"  :: (T_Addr) @ t_1891 -> (T_Addr) @ t_1892 -> {"State#" {RealWorld} @ t_1893} @ t_1894 -> {"ghc-prim_GHC.Prim.(#,#)" {"Compact#"} @ t_1895 (T_Addr) @ t_1896} @ t_1897
    "compactAdd#"            :: {"Compact#"} @ t_1898 -> %a_86 -> {"State#" {RealWorld} @ t_1899} @ t_1900 -> {"ghc-prim_GHC.Prim.Unit#" %a_86} @ t_1901
    "compactAddWithSharing#" :: {"Compact#"} @ t_1902 -> %a_87 -> {"State#" {RealWorld} @ t_1903} @ t_1904 -> {"ghc-prim_GHC.Prim.Unit#" %a_87} @ t_1905
    "compactSize#"           :: {"Compact#"} @ t_1906 -> {"State#" {RealWorld} @ t_1907} @ t_1908 -> {"ghc-prim_GHC.Prim.Unit#" (T_Word64) @ t_1909} @ t_1910

  {-
    Unsafe pointer equality
  -}
  primop pure
    "reallyUnsafePtrEquality#" :: %a_88 -> %a_88 -> (T_Int64) @ t_1911

  {-
    Parallelism
  -}
  primop effectful
    "par#"   :: %a_89 -> (T_Int64) @ t_1912
    "spark#" :: %a_90 -> {"State#" %s_184} @ t_1913 -> {"ghc-prim_GHC.Prim.Unit#" %a_90} @ t_1914

  primop pure
    "seq#" :: %a_91 -> {"State#" %s_185} @ t_1915 -> {"ghc-prim_GHC.Prim.Unit#" %a_91} @ t_1916

  primop effectful
    "getSpark#"  :: {"State#" %s_186} @ t_1917 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_1918 %a_92} @ t_1919
    "numSparks#" :: {"State#" %s_187} @ t_1920 -> {"ghc-prim_GHC.Prim.Unit#" (T_Int64) @ t_1921} @ t_1922

  {-
    Tag to enum stuff
  -}
  primop pure
    "dataToTag#" :: %a_93 -> (T_Int64) @ t_1923
    "tagToEnum#" :: (T_Int64) @ t_1924 -> %a_94

  {-
    Bytecode operations
  -}
  primop pure
    "addrToAny#" :: (T_Addr) @ t_1925 -> {"ghc-prim_GHC.Prim.Unit#" %a_95} @ t_1926
    "anyToAddr#" :: %a_96 -> {"State#" {RealWorld} @ t_1927} @ t_1928 -> {"ghc-prim_GHC.Prim.Unit#" (T_Addr) @ t_1929} @ t_1930
    "mkApUpd0#"  :: {BCO} @ t_1931 -> {"ghc-prim_GHC.Prim.Unit#" %a_97} @ t_1932

  primop effectful
    "newBCO#" :: {"ByteArray#"} @ t_1933 -> {"ByteArray#"} @ t_1934 -> {"Array#" %a_98} @ t_1935 -> (T_Int64) @ t_1936 -> {"ByteArray#"} @ t_1937 -> {"State#" %s_188} @ t_1938 -> {"ghc-prim_GHC.Prim.Unit#" {BCO} @ t_1939} @ t_1940

  primop pure
    "unpackClosure#" :: %a_99 -> {"ghc-prim_GHC.Prim.(#,,#)" (T_Addr) @ t_1941 {"ByteArray#"} @ t_1942 {"Array#" %b_9} @ t_1943} @ t_1944
    "closureSize#"   :: %a_100 -> (T_Int64) @ t_1945
    "getApStackVal#" :: %a_101 -> (T_Int64) @ t_1946 -> {"ghc-prim_GHC.Prim.(#,#)" (T_Int64) @ t_1947 %b_10} @ t_1948

  {-
    Misc
  -}
  primop pure
    "getCCSOf#"      :: %a_102 -> {"State#" %s_189} @ t_1949 -> {"ghc-prim_GHC.Prim.Unit#" (T_Addr) @ t_1950} @ t_1951
    "getCurrentCCS#" :: %a_103 -> {"State#" %s_190} @ t_1952 -> {"ghc-prim_GHC.Prim.Unit#" (T_Addr) @ t_1953} @ t_1954
    "clearCCS#"      :: (tf_1252 : {"State#" %s_191} @ t_1955 -> {"ghc-prim_GHC.Prim.Unit#" %a_104} @ t_1956) -> {"State#" %s_191} @ t_1957 -> {"ghc-prim_GHC.Prim.Unit#" %a_104} @ t_1958

  {-
    Etc
  -}
  primop effectful
    "traceEvent#"                 :: (T_Addr) @ t_1959 -> {"State#" %s_192} @ t_1960 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1961
    "traceBinaryEvent#"           :: (T_Addr) @ t_1962 -> (T_Int64) @ t_1963 -> {"State#" %s_193} @ t_1964 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1965
    "traceMarker#"                :: (T_Addr) @ t_1966 -> {"State#" %s_194} @ t_1967 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1968
    "setThreadAllocationCounter#" :: (T_Int64) @ t_1969 -> {"State#" {RealWorld} @ t_1970} @ t_1971 -> {"ghc-prim_GHC.Prim.(##)"} @ t_1972

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
    "prefetchByteArray3#"        :: {"ByteArray#"} @ t_2001 -> (T_Int64) @ t_2002 -> {"State#" %s_201} @ t_2003 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2004
    "prefetchMutableByteArray3#" :: {"MutableByteArray#" %s_202} @ t_2005 -> (T_Int64) @ t_2006 -> {"State#" %s_202} @ t_2007 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2008
    "prefetchAddr3#"             :: (T_Addr) @ t_2009 -> (T_Int64) @ t_2010 -> {"State#" %s_203} @ t_2011 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2012
    "prefetchValue3#"            :: %a_105 -> {"State#" %s_204} @ t_2013 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2014
    "prefetchByteArray2#"        :: {"ByteArray#"} @ t_2015 -> (T_Int64) @ t_2016 -> {"State#" %s_205} @ t_2017 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2018
    "prefetchMutableByteArray2#" :: {"MutableByteArray#" %s_206} @ t_2019 -> (T_Int64) @ t_2020 -> {"State#" %s_206} @ t_2021 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2022
    "prefetchAddr2#"             :: (T_Addr) @ t_2023 -> (T_Int64) @ t_2024 -> {"State#" %s_207} @ t_2025 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2026
    "prefetchValue2#"            :: %a_106 -> {"State#" %s_208} @ t_2027 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2028
    "prefetchByteArray1#"        :: {"ByteArray#"} @ t_2029 -> (T_Int64) @ t_2030 -> {"State#" %s_209} @ t_2031 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2032
    "prefetchMutableByteArray1#" :: {"MutableByteArray#" %s_210} @ t_2033 -> (T_Int64) @ t_2034 -> {"State#" %s_210} @ t_2035 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2036
    "prefetchAddr1#"             :: (T_Addr) @ t_2037 -> (T_Int64) @ t_2038 -> {"State#" %s_211} @ t_2039 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2040
    "prefetchValue1#"            :: %a_107 -> {"State#" %s_212} @ t_2041 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2042
    "prefetchByteArray0#"        :: {"ByteArray#"} @ t_2043 -> (T_Int64) @ t_2044 -> {"State#" %s_213} @ t_2045 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2046
    "prefetchMutableByteArray0#" :: {"MutableByteArray#" %s_214} @ t_2047 -> (T_Int64) @ t_2048 -> {"State#" %s_214} @ t_2049 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2050
    "prefetchAddr0#"             :: (T_Addr) @ t_2051 -> (T_Int64) @ t_2052 -> {"State#" %s_215} @ t_2053 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2054
    "prefetchValue0#"            :: %a_108 -> {"State#" %s_216} @ t_2055 -> {"ghc-prim_GHC.Prim.(##)"} @ t_2056

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
