-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Bittide.Simulate where

import Clash.Explicit.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.Simulate

createDomain vXilinxSystem{vPeriod=hzToPeriod 200e6, vName="Fast"}
createDomain vXilinxSystem{vPeriod=hzToPeriod 20e6, vName="Slow"}

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
  let
    config = defClockConfig
    dataCounts = pure maxBound :> Nil
    changes =
      sampleN
        -- +10_000 assumes callisto's pipeline less than 10_000 deep
        (fromIntegral (cccPessimisticSettleCycles config + 10_000))
        (callistoClockControl @_ @_ @Fast clockGen resetGen enableGen config dataCounts)

  assertBool
    "only requests speed up"
    (SpeedUp `elem` changes && SlowDown `notElem` changes)

case_clockControlMinBound :: Assertion
case_clockControlMinBound = do
  let
    config = defClockConfig
    dataCounts = pure 0 :> Nil
    changes =
      sampleN
        -- +100 assumes callisto's pipeline less than 100 deep
        (fromIntegral (cccPessimisticSettleCycles config + 100))
        (callistoClockControl @_ @_ @Fast clockGen resetGen enableGen config dataCounts)

  assertBool
    "only requests slow down"
    (SlowDown `elem` changes && SpeedUp `notElem` changes)

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMaxBound :: Assertion
case_elasticBufferMaxBound = do
  let dataCounts = sampleN 2048 (elasticBuffer @5 Saturate (clockGen @Slow) (clockGen @Fast))
  -- it never hits exactly the maximum because the occupancy is in the read
  -- domain, i.e. we have to look for one less than the max
  assertBool "elastic buffer should reach its near maximum (read domain)" ((maxBound-1) `elem` dataCounts)
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMinBound :: Assertion
case_elasticBufferMinBound = do
  let dataCounts = sampleN 1024 (elasticBuffer @5 Saturate (clockGen @Fast) (clockGen @Slow))
  assertBool "elastic buffer should reach its minimum" (0 `elem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (maxBound `notElem` dataCounts)

-- | When the elasticBuffer written to as quikly to as it is read from, it should
-- reach neiher its maxBound nor minBound.
case_elasticBufferEq :: Assertion
case_elasticBufferEq = do
  let dataCounts = sampleN 1024 (elasticBuffer @5 Saturate (clockGen @Slow) (clockGen @Slow))
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (maxBound `notElem` dataCounts)
