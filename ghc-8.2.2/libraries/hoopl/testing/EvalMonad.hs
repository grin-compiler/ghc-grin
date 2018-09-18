{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns , FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module EvalMonad (ErrorM, VarEnv, B, State,
                  EvalM, runProg, inNewFrame, get_proc, get_block,
                         get_var, set_var, get_heap, set_heap,
                  Event (..), event) where

import Control.Applicative as AP (Applicative(..))
import Control.Monad.Except
import qualified Data.Map as M
import Prelude hiding (succ)

import Compiler.Hoopl hiding ((<*>))
import IR

type ErrorM        = Either String
type InnerErrorM v = Either (State v, String)

data EvalM v a = EvalM (State v -> InnerErrorM v (State v, a))

instance Monad (EvalM v) where
  return = AP.pure
  EvalM f >>= k = EvalM $ \s -> do (s', x) <- f s
                                   let EvalM f' = k x
                                   f' s'

instance Functor (EvalM v) where
  fmap = liftM

instance Applicative (EvalM v) where
  pure x = EvalM (\s -> return (s, x))
  (<*>) = ap

instance MonadError String (EvalM v) where
  throwError e = EvalM (\s -> throwError (s, e))
  catchError (EvalM f) handler =
    EvalM $ \s -> f s `catchError` handler'
                where handler' (s', e) = let EvalM f' = handler e
                                         in  f' s'

-- Shorthands for frequently used types
type VarEnv  v = M.Map Var  v
type HeapEnv v = M.Map Addr v -- word addressed heap
type Addr      = Integer
type B         = Block Insn C C
type PEnv      = M.Map String Proc
type G         = Graph Insn C C

runProg :: [Proc] -> [v] -> EvalM v x -> ErrorM (State v, x)
runProg procs vs (EvalM f) =
  case f init_state of
    Left (_, e) -> throwError e
    Right x     -> return x
  where
    init_state = State { frames = [], heap = M.empty, events = [],
                         vsupply = vs, procs = procMap }
    procMap = M.fromList $ zip (map name procs) procs

get_state :: EvalM v (State v)
get_state = EvalM f
  where f state = return (state, state)

upd_state :: (State v -> State v) -> EvalM v ()
upd_state upd = EvalM (\state -> return (upd state, ()))

event :: Event v -> EvalM v ()
event e = upd_state (\s -> s {events = e : events s})

----------------------------------
-- State of the machine
data State v = State { frames  :: [(VarEnv v, G)]
                     , heap    :: HeapEnv v
                     , procs   :: PEnv
                     , vsupply :: [v]
                     , events  :: [Event v]
                     }
data Event v = CallEvt String [v]
             | RetEvt         [v]
             | StoreEvt Addr v
             | ReadEvt  Addr v

get_var :: Var -> EvalM v v
get_var var = get_state >>= k
  where k (State {frames = (vars, _):_}) = mlookup "var" var vars
        k _ = throwError "can't get vars from empty stack"

set_var :: Var -> v -> EvalM v ()
set_var var val = upd_state f
  where f s@(State {frames = (vars, blocks):vs}) =
            s { frames = (M.insert var val vars, blocks):vs }
        f _ = error "can't set var with empty stack"

-- Special treatment for the heap:
-- If a heap location doesn't have a value, we give it one.
get_heap :: Addr -> EvalM v v
get_heap addr =
  do State {heap, vsupply} <- get_state
     (v, vs) <- case vsupply of v:vs -> return (v, vs)
                                _    -> throwError "hlookup hit end of value supply"
     upd_state (\s -> s {heap = M.insert addr v heap, vsupply = vs})
     event $ ReadEvt addr v
     return v

set_heap :: Addr -> v -> EvalM v ()
set_heap addr val =
  do event $ StoreEvt addr val
     upd_state $ \ s -> s { heap = M.insert addr val (heap s) }

get_block :: Label -> EvalM v B
get_block lbl = get_state >>= k
  where k (State {frames = (_, graph):_}) = blookup "block" graph lbl
        k _ = error "can't get blocks from empty stack"

get_proc :: String -> EvalM v Proc
get_proc name = get_state >>= mlookup "proc" name . procs

newFrame :: VarEnv v -> G -> EvalM v ()
newFrame vars graph = upd_state $ \s -> s { frames = (vars, graph) : frames s}

popFrame :: EvalM v ()
popFrame = upd_state f
  where f s@(State {frames = _:fs}) = s { frames = fs }
        f _ = error "popFrame: no frame to pop..." -- implementation error

inNewFrame :: VarEnv v -> G -> EvalM v x -> EvalM v x
inNewFrame vars graph runFrame =
  do newFrame vars graph
     x <- runFrame
     popFrame
     return x

mlookup :: Ord k => String -> k -> M.Map k v -> EvalM v' v
mlookup blame k m =
  case M.lookup k m of
    Just v  -> return v
    Nothing -> throwError ("unknown lookup for " ++ blame)

blookup :: String -> G -> Label -> EvalM v B
blookup blame (GMany _ blks _) lbl =
  case mapLookup lbl blks of
    Just b  -> return b
    Nothing -> throwError ("unknown lookup for " ++ blame)
