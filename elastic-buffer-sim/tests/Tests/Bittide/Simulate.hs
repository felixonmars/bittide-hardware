{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Bittide.Simulate where

import Clash.Explicit.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.Simulate

createDomain vXilinxSystem{vPeriod=11, vName="Fast"}
createDomain vXilinxSystem{vPeriod=13, vName="Slow"}

tests :: TestTree
tests = testGroup "Simulate"
  [ testGroup "elasticBuffer"
    [ testCase "case_elasticBufferMaxBound" case_elasticBufferMaxBound
    , testCase "case_elasticBufferMinBound" case_elasticBufferMinBound
    , testCase "case_elasticBufferEq" case_elasticBufferEq
    , testCase "case_caseClockControlMaxBound" case_clockControlMaxBound
    , testCase "case_caseClockControlMinBound" case_clockControlMinBound
    ]
  ]

case_clockControlMaxBound :: Assertion
case_clockControlMaxBound = do
  let (change:_) =
        sampleN 1024 (clockControl @_ @Fast 1 (-1) 1 128 (pure 128 :> Nil))
  change @?= SpeedUp

case_clockControlMinBound :: Assertion
case_clockControlMinBound = do
  let (change:_) =
        sampleN 1024 (clockControl @_ @Fast 1 (-1) 1 128 (pure 0 :> Nil))
  change @?= SlowDown

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMaxBound :: Assertion
case_elasticBufferMaxBound = do
  let dataCounts = sampleN 1024 (elasticBuffer 32 (clockGen @Slow) (clockGen @Fast))
  assertBool "elastic buffer should reach its maximum" (32 `elem` dataCounts)
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMinBound :: Assertion
case_elasticBufferMinBound = do
  let dataCounts = sampleN 1024 (elasticBuffer 32 (clockGen @Fast) (clockGen @Slow))
  assertBool "elastic buffer should reach its minimum" (0 `elem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (32 `notElem` dataCounts)

-- | When the elasticBuffer written to as quikly to as it is read from, it should
-- reach neiher its maxBound nor minBound.
case_elasticBufferEq :: Assertion
case_elasticBufferEq = do
  let dataCounts = sampleN 1024 (elasticBuffer 32 (clockGen @Slow) (clockGen @Slow))
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (32 `notElem` dataCounts)
