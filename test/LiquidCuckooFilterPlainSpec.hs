{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LiquidCuckooFilterPlainSpec(spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Data.LiquidCuckooFilterPlain
import Data.LiquidCuckooFilter.Entities
import Data.LiquidCuckooFilter.Internal

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Test.QuickCheck

bucketNonEmpty = B (BS.pack [fp, fp0])
  where
    (FP fp) = makeFingerprint ("asd" :: Text)
    fp0 = 0

bucketFull1 = B (BS.pack [fp0, fp0])
  where
    (FP fp0) = makeFingerprint ("a" :: Text)

bucketFull2 = B (BS.pack [fp0, fp0])
  where
    (FP fp0) = makeFingerprint ("b" :: Text)

halfFullTable = V.fromList [bucketFull1, bucketNonEmpty, bucketFull2, bucketNonEmpty]

emptyDefaultFilter :: Table String
emptyDefaultFilter = initialize totalBuckets slots maxNumKicks
  where
    totalBuckets = 4
    slots = 2
    maxNumKicks = 20

halfFullFilter :: Table String
halfFullFilter = Table {
  buckets = halfFullTable,
  slots = 2,
  totalBuckets = V.length halfFullTable,
  maxNumKicks = 20
}

spec :: IO ()
spec = hspec $ do
  
  describe "attempts to update the filled in bucket" $ do

    it "should terminate" $
      let x = attemptToWriteFingerprint 0 (FP 1, 1) emptyDefaultFilter 
      in x `shouldBe` Nothing

    it "should relocate if there is a chance" $
      let 
        (FP fpInserted) = makeFingerprint ("a" :: Text)
        (FP fpExisted) = makeFingerprint ("asd" :: Text)
        x = attemptToWriteFingerprint 20 ((FP fpInserted), 100) halfFullFilter 
        Just Table{buckets} = x
        updatedBucket1 = (B (BS.pack [fpExisted, fpInserted]))
        updatedBucket2 = (B (BS.pack [fpExisted, 0]))
      in 
        buckets `shouldBe` V.fromList [bucketFull1, updatedBucket1, bucketFull2, updatedBucket2]

    it "should relocate if there is a chance 2" $
      let 
        slots = 2
        emptyB = emptyBucket slots
        (FP fpInserted) = makeFingerprint ("a" :: Text)
        initialBuckets = V.fromList [bucketFull1, emptyB, bucketFull2, emptyB]
        filter = Table {
          buckets = initialBuckets,
          slots = slots,
          totalBuckets = V.length initialBuckets,
          maxNumKicks = 20
        }
        x = attemptToWriteFingerprint 20 ((FP fpInserted), 100) filter 
        Just Table{buckets} = x
        updatedBucket = (B (BS.pack [fpInserted, 0]))
      in
        buckets `shouldBe` V.fromList [bucketFull1, updatedBucket, bucketFull2, emptyB]
  
  describe "member" $ do

    it "cannot find non-existing value" $
      property $ \s ->
        not (member emptyDefaultFilter s)

  describe "delete" $ do

    it "does not crash to delete non-existing value" $
      property $ \s ->
        (delete emptyDefaultFilter s) `shouldBe` emptyDefaultFilter
  
  describe "property tests" $ do

    it "insert and then delete" $ 
      property $ \s -> 
        let
          Just f = insert emptyDefaultFilter s
          f' = delete f s
        in
          not (member f' s)

    it "insert and member" $ 
      property $ \s -> 
        let Just f = insert emptyDefaultFilter s
        in member f s
