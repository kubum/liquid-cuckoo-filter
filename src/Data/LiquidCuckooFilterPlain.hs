{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--ple" @-}

module Data.LiquidCuckooFilterPlain
  (
    Table(..)
    , initialize
    , insert
    , member
    , delete
    , attemptToWriteFingerprint
  ) where

import Data.LiquidCuckooFilter.Entities
import Data.LiquidCuckooFilter.Internal
import Data.LiquidCuckooFilter.Random(randomPick, evictRandomFingeprint)

import qualified Data.Vector as V
import Data.Hashable (Hashable, hash)
import Data.Bits (xor)

{-@ V.update :: v1: V.Vector a 
             -> v2: V.Vector (Int, a) 
             -> { v3: V.Vector a | vlen v1 == vlen v3 } @-}
{-@ type PositiveInt = { v: Int | v > 0 } @-}
{-@ type BucketN N = { v : V.Vector Bucket | vlen v = N } @-}

-- | A Cuckoo Filter hash table
{-@ 
  data Table m = Table {
                        slots :: PositiveInt
                      , totalBuckets :: PositiveInt
                      , maxNumKicks :: PositiveInt
                      , buckets :: BucketN totalBuckets
                    
 } @-}
data Table m = Table {
  slots :: !Int,
  totalBuckets :: !Int,
  maxNumKicks :: !Int,
  buckets :: V.Vector Bucket
} deriving (Show, Eq)

{-@ defaultBuckets :: v : PositiveInt -> PositiveInt -> BucketN v @-}
defaultBuckets :: Int -> Int -> V.Vector Bucket
defaultBuckets totalBuckets slots = V.fromList $ replicate totalBuckets (emptyBucket slots)

{-@ initialize :: PositiveInt -> PositiveInt -> PositiveInt -> Table a @-}
initialize :: Int -> Int -> Int -> Table a
initialize totalBuckets slots maxNumKicks = 
  Table {
      slots = slots,
      totalBuckets = totalBuckets,
      maxNumKicks = maxNumKicks,
      buckets = defaultBuckets totalBuckets slots
  }

slotsCount :: Table a -> Int
slotsCount Table {slots} = slots

getMaxNumKicks :: Table a -> Int
getMaxNumKicks Table {maxNumKicks} = maxNumKicks

writeBucket :: Table a -> Bucket -> Int -> Table a
writeBucket filter @ (Table {buckets}) bucket index =
  filter {buckets = V.update buckets (V.fromList [(index, bucket)])}

{-@ toIndex :: t: Table a -> Int -> { x: Int | x >=0 && x < vlen (buckets t) } @-}
toIndex :: Table a -> Int -> Int
toIndex Table {totalBuckets} hash = hash `mod` totalBuckets

{-@ readBucket :: t: Table a -> { v: Int | v >= 0 && v < vlen (buckets t) } -> Bucket @-}
readBucket :: Table a -> Int -> Bucket
readBucket Table {buckets} index = buckets V.! index

insert :: (Hashable element) => Table element -> element -> Maybe (Table element)
insert filter element =
  let 
    slots = slotsCount filter
    f = makeFingerprint element
    h1 = hash element
    h2 = h1 `xor` (hash f)
    index1 = toIndex filter h1 
    index2 = toIndex filter h2 
    bucketA = readBucket filter index1
    bucketB = readBucket filter index2
  in case preferableEmptyBucket slots (index1, bucketA) (index2, bucketB) of
    Just (bucketPosition, bucket, availableSlot) -> Just $ writeBucket filter (setSlot bucket availableSlot f) bucketPosition
    Nothing ->
      attemptToWriteFingerprint maxNumKicks (f, randomHash) filter
      where
        maxNumKicks = getMaxNumKicks filter
        randomHash = randomPick h1 h2

member :: (Hashable element) => Table element -> element -> Bool
member filter element =
  let
    f = makeFingerprint element
    h1 = hash element
    h2 = h1 `xor` (hash f)
    index1 = toIndex filter h1
    index2 = toIndex filter h2
    bucketA = readBucket filter index1
    bucketB = readBucket filter index2
  in ((contains bucketA f) || (contains bucketB f))

delete :: (Hashable element) => Table element -> element -> Table element
delete filter element =
  let
    f = makeFingerprint element
    h1 = hash element
    h2 = h1 `xor` (hash f)
    index1 = toIndex filter h1
    index2 = toIndex filter h2
    bucketA = readBucket filter index1
    bucketB = readBucket filter index2
  in
    case (clean bucketA f) of
    Just bucket -> writeBucket filter bucket index1 
    Nothing -> 
      case (clean bucketB f) of
        Just bucket -> writeBucket filter bucket index2
        Nothing -> filter
  where 
    clean bucket fp = if (contains bucket fp) then Just (remove bucket fp) else Nothing

{-@ attemptToWriteFingerprint :: n: Nat -> (Fingerprint, Int) -> Table a -> Maybe (Table a) / [n] @-}
attemptToWriteFingerprint :: Int -> (Fingerprint, Int) -> Table a -> Maybe (Table a)
attemptToWriteFingerprint 0 _ _ = Nothing
attemptToWriteFingerprint remainingKicks (fp, h1) filter =
  let slots = slotsCount filter
      index1 = toIndex filter h1
      bucket = readBucket filter index1
      (evictedIndex, evictedFingerprint) = evictRandomFingeprint bucket
      bucket' = setSlot bucket evictedIndex fp
      h2 = h1 `xor` (hash evictedFingerprint)
      filter' = writeBucket filter bucket' index1
      index2 = toIndex filter' h2
      bucket'' = readBucket filter' index2
  in case nextSlotBucket slots bucket'' of
    Just slot -> Just $ writeBucket filter' (setSlot bucket'' slot evictedFingerprint) index2 
    Nothing -> attemptToWriteFingerprint (remainingKicks - 1) (evictedFingerprint, h2) filter'
