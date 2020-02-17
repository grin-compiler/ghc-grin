import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

main :: IO ()
main = (`mapM_` [1..1000]) $ \_ -> do
  traceEventIO "(((("

  x <- newTVarIO False

  forkIO $ atomically $ writeTVar x True

  traceEventIO "----"

  atomically $ do  -- hangs in the second iteration
    _ <- readTVar x
    writeTVar x True `orElse` return ()

  threadDelay 100000
  traceEventIO "))))"
