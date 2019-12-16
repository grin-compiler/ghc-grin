module Lambda.GHCSymbols where

liveSymbols =
  -- base
  [ "Control.Exception.Base.absentSumFieldError"
  , "Control.Exception.Base.nestedAtomically"
  , "Control.Exception.Base.nonTermination"
  , "GHC.IO.Exception.allocationLimitExceeded"
  , "GHC.IO.Exception.blockedIndefinitelyOnMVar"
  , "GHC.IO.Exception.blockedIndefinitelyOnSTM"
  , "GHC.IO.Exception.cannotCompactFunction"
  , "GHC.IO.Exception.cannotCompactMutable"
  , "GHC.IO.Exception.cannotCompactPinned"
  , "GHC.IO.Exception.heapOverflow"
  , "GHC.IO.Exception.stackOverflow"
  , "GHC.Event.Thread.blockedOnBadFD"

  , "GHC.Conc.IO.ensureIOManagerIsRunning"
  , "GHC.Conc.IO.ioManagerCapabilitiesChanged"
  , "GHC.Conc.Signal.runHandlersPtr"
  , "GHC.Conc.Sync.runSparks"
  , "GHC.Pack.unpackCString"
  , "GHC.TopHandler.flushStdHandles"
  , "GHC.TopHandler.runIO"
  , "GHC.TopHandler.runMainIO"
  , "GHC.TopHandler.runNonIO"
  , "GHC.Weak.runFinalizerBatch"

  -- ghc-prim
  , "GHC.Tuple.()"
  , "GHC.Types.False"
  , "GHC.Types.True"
  ]

{-
  Q:  what is con_info? is it callable code?
      how is it generated?
      does it have stg representation?


         GHC.Int_I16zh_con_info
         GHC.Int_I32zh_con_info
         GHC.Int_I64zh_con_info
         GHC.Int_I8zh_con_info
         GHC.Ptr_FunPtr_con_info
         GHC.Ptr_Ptr_con_info
         GHC.Stable_StablePtr_con_info
         GHC.Word_W16zh_con_info
         GHC.Word_W32zh_con_info
         GHC.Word_W64zh_con_info
         GHC.Word_W8zh_con_info

         ghczmprim_GHC.Types_Czh_con_info
         ghczmprim_GHC.Types_Izh_con_info
         ghczmprim_GHC.Types_Fzh_con_info
         ghczmprim_GHC.Types_Dzh_con_info
         ghczmprim_GHC.Types_Wzh_con_info
-}
