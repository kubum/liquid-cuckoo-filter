{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.LiquidCuckooFilter.Entities (
  Fingerprint(..), 
  Bucket(..), 
  emptyFP, 
  emptyBucket,
  getSize,
  getSlot,
  setSlot,
  isEmpty,
  contains,
  remove,
) where 

import qualified Data.ByteString as BS
import Data.ByteString.Unsafe(unsafeIndex, unsafeDrop, unsafeTake)

import Data.Hashable (Hashable, hash)
import Data.Serialize (Serialize)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- | A Fingerprint is an 8 bit hash of a value
newtype Fingerprint = FP Word8
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)
  deriving anyclass Serialize

emptyFP :: Fingerprint
emptyFP = FP 0

{-@ data Bucket = B { content :: BS.ByteString } @-}
data Bucket = B { content :: BS.ByteString } deriving (Show, Eq)

{-@ getSize :: b: Bucket -> { v: Int | bslen (content b) = v }  @-}
getSize :: Bucket -> Int
getSize (B bucket) = BS.length bucket

isEmpty :: Bucket -> Bool
isEmpty (B bucket) = BS.null bucket

emptyBucket :: Int -> Bucket 
emptyBucket slots = B (BS.replicate slots 0)

{-@ getSlot :: b: Bucket 
            -> { index: Int | index >= 0 && bslen (content b) > index } 
            -> Fingerprint @-}
getSlot :: Bucket -> Int -> Fingerprint
getSlot (B bucket) id = FP (unsafeIndex bucket id)

{-@ setSlot :: b: Bucket 
            -> { index: Int | index >= 0 && bslen (content b) > index } 
            -> Fingerprint
            -> Bucket @-}
setSlot :: Bucket -> Int -> Fingerprint -> Bucket
setSlot (B bucket) id (FP fp) = B (BS.concat [before, addedBytes, after])
  where
    (before, rest) = unsafeSplitAt id bucket
    after = unsafeDrop 1 rest
    addedBytes = BS.pack [fp]

{-@ unsafeSplitAt
    ::  v: { v: Int | v >=0 }
    ->  b: { bs : BS.ByteString | bslen bs >= v }
    ->  ( { bs : BS.ByteString | bslen bs == v } , { bs : BS.ByteString | bslen bs == bslen b - v } ) @-}
unsafeSplitAt :: Int -> BS.ByteString -> (BS.ByteString, BS.ByteString)
unsafeSplitAt n bs = (unsafeTake n bs, unsafeDrop n bs)

contains :: Bucket -> Fingerprint -> Bool
contains (B bucket) (FP fp) = BS.elem fp bucket

remove :: Bucket -> Fingerprint -> Bucket
remove (B bucket) (FP fp) = B (BS.filter (\f -> f /= fp) bucket)
