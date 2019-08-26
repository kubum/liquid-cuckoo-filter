module Data.LiquidCuckooFilter.Random (
  randomPick,
  evictRandomFingeprint,
) where

import System.Random (randomIO, randomRIO)
import System.IO.Unsafe (unsafePerformIO)

import Data.LiquidCuckooFilter.Entities

-- For now we believe it is safe to trust the IO nature of random generator, and ignore referential transparency
randomPick :: a -> a -> a
randomPick h1 h2 = unsafePerformIO $ pick
  where 
    pick = do
      bool <- randomIO :: IO Bool
      pure (if bool then h1 else h2)

evictRandomFingeprint :: Bucket -> (Int, Fingerprint)
evictRandomFingeprint bucket = unsafePerformIO $ pick
  where 
    pick = do
      i <- randomRIO (0, (getSize bucket) - 1)
      pure (i, getSlot bucket i) -- it could be an empty bucket