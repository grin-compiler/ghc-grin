import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.Environment

main :: IO ()
main = do
    let volume = 1000
    hSetBuffering stdout NoBuffering
    [n] <- fmap read <$> getArgs
    forM_ [1..(n::Int)] $ \i -> do
        putStr $ show i ++ " "

        -- Spawn massive number of threads.
        threads <- replicateM volume $ do
            trigger <- newTVarIO False
            tid <- forkIO $ void $ atomically $ do
                t <- readTVar trigger
                if t then pure t else retry
            pure (trigger, tid)

        -- Make sure all threads are spawned.
        threadDelay 30000

        -- Let threads start to exit normally.
        forkIO $ forM_ threads $ \(trigger, _) -> threadDelay 1 *> atomically (writeTVar trigger True)

        -- Concurrently kill threads in order to create race.
        -- TMVar operation and asynchronous exception can hit same thread simultaneously.
        -- Adjust threadDelay if you don't reproduce very well.
        threadDelay 1000
        forM_ threads $ \(_, tid) -> do
            -- putChar 'A'
            killThread tid      -- When the issue reproduced, this killThread doesn't return.
            -- putChar '\b'

