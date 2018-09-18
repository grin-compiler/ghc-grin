module Hello where

--import Data.Map
import Control.Monad.State

f :: String
f = evalState (pure "hello") ()

data MyIntList
  = IntListCons Int
  | IntListNil

procMyIntList :: MyIntList -> Maybe Int
procMyIntList (IntListCons i) = Just i
procMyIntList IntListNil = Nothing

myId :: MyIntList -> MyIntList
myId (IntListCons _) = IntListNil
myId IntListNil = IntListCons 0

