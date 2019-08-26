{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.LiquidCuckooFilter.Internal (
  makeFingerprint,  
  getSize,
  getSlot,
  setSlot,
  contains,
  remove,
  preferableEmptyBucket,
  nextSlotBucket,
) where

import Data.LiquidCuckooFilter.Entities
import Data.LiquidCuckooFilter.Random(evictRandomFingeprint)

import qualified Data.ByteString as BS

import Data.Bits (xor)
import Data.Hashable (Hashable, hash)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)

nextSlotBucket :: Int -> Bucket -> Maybe Int
nextSlotBucket slots bucket
    | isEmpty bucket = Just 0
    | otherwise      = nextSlotInNonEmptyBucket slots bucket

nextSlotInNonEmptyBucket :: Int -> Bucket -> Maybe Int
nextSlotInNonEmptyBucket slots (B bucket) = BS.findIndex (\b -> b == 0) bucket

preferableEmptyBucket :: Int -> (Int, Bucket) -> (Int, Bucket) -> Maybe (Int, Bucket, Int)
preferableEmptyBucket slots (bucketIndexA, bucketA) (bucketIndexB, bucketB) =
    case emptySlotInA of
        Just slotIndex -> Just (bucketIndexA, bucketA, slotIndex)
        Nothing -> (\slotIndex -> (bucketIndexB, bucketB, slotIndex)) <$> emptySlotInB  
    where 
        availability bucket = if isEmpty bucket 
            then Just 0
            else nextSlotInNonEmptyBucket slots bucket
        emptySlotInA = availability bucketA
        emptySlotInB = availability bucketB
    
makeFingerprint :: Hashable a => a -> Fingerprint
makeFingerprint a = FP . max 1 $ fromIntegral (abs $ hash a) `mod` 255
