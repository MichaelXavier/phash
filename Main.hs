module Main (main) where

import Control.Concurrent.Async
import System.Environment
import PHash

main :: IO ()
main = do
  (path:_) <- getArgs
  -- mapM imageHash (replicate 10 path)
  mapConcurrently imageHash (replicate 50 path)
  return ()
  -- (path1:path2:_) <- getArgs
  -- (Just ph1) <- imageHash path1
  -- (Just ph2) <- imageHash path2
  -- print $ hammingDistance ph1 ph2

-- runhaskell -Wall -threaded -lpthread -IpHash-0.9.6/src/ -LpHash-0.9.6/src/.libs -lpHash Main.hs /home/michael/Dropbox/Photos/IMAG0174.jpg /home/michael/Dropbox/Photos/IMAG0202.jpg
