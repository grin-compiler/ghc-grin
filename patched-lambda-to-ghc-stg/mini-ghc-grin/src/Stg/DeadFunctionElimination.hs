{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, OverloadedStrings #-}
module Stg.DeadFunctionElimination where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import Control.Monad

import System.IO
import System.FilePath
import System.Directory

import Stg.Syntax


{-
  collect top level names defs <---> top-level/external names references

  to collect:
    - top-name => [referred top level name]
    - top-name => [referred type con]
    - top-name => [referred data con]

  TODO:
    done - datalog analysis

    done - save facts for an stgbin
    done - use globally unique names (program global)
    done - export live fun sources i.e. foreign exported top level names

  PIPELINE:
    - generate facts for stgbin if it does not exists [cached]
    - feed facts to datalog
      Q: use haskell souffle or not?
      A: try haskell souffle, use it!
    - write result file for each stgbin
    done - use liveness result in the concurrent code generator
-}

