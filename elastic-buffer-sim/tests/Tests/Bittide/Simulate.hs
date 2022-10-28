-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Bittide.Simulate where

import Clash.Explicit.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.ClockControl
import Bittide.ClockControl.Strategies
import Bittide.Simulate
import Bittide.Simulate.Ppm
import Bittide.Topology.TH.Domain

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
    , testCase "case_readDomTerminates" case_readDomTerminates
    , testCase "case_directEbsTerminates" case_directEbsTerminates
    , testCase "case_fullNodeTerminates" case_fullNodeTerminates
    ]
  ]

fastPeriod :: PeriodPs
fastPeriod = hzToPeriod 200e6

clockConfig :: Ppm -> ClockControlConfig
clockConfig clockUncertainty = ClockControlConfig
  { cccPessimisticPeriod = speedUpPeriod clockUncertainty fastPeriod
  , cccSettlePeriod      = fastPeriod * 200
  , cccDynamicRange      = clockUncertainty * 2
  , cccStepSize          = 10
  , cccBufferSize        = 128
  }

case_readDomTerminates :: Assertion
case_readDomTerminates =
  assertBool "doesn't <<loop>>" (ebRdOut `deepseqX` True)
 where
  ebRdOut = sampleN 1000 ebRd
  ebRd =
    ebReadDom @Fast @Slow clockGen clockGen resetGen resetGen enableGen enableGen (pure Wait)

case_directEbsTerminates :: Assertion
case_directEbsTerminates =
  assertBool "doesn't <<loop>>" (outSample `deepseqX` True)
 where
  ebCtl = ebReadDom @Fast @Slow clockGen clockGen resetGen resetGen enableGen enableGen
  (ebRst, ebDat :> Nil) = directEbs clockGen resetGen enableGen (ebCtl :> Nil)
  outSample = sampleN 5 ebRst

case_fullNodeTerminates :: Assertion
case_fullNodeTerminates =
  assertBool "doesn't <<loop>>" (outSample `deepseqX` True)
 where
  outSample = sampleN 1000 ccRst0
  (ccRst0, ebNode0) = directEbs @Bittide clock0 resetGen enableGen (ebReadDom clock0 clock1 resetGen resetGen enableGen enableGen :> Nil)
  (ccRst1, ebNode1) = directEbs @Bittide clock1 resetGen enableGen (ebReadDom clock1 clock0 resetGen resetGen enableGen enableGen :> Nil)
  clock0 = tunableClockGen 999900000 0 1 resetGen clockControl0
  clock1 = tunableClockGen 999900000 10000 1 resetGen clockControl1
  clockControl0 = callistoClockControl clock0 (unsafeFromHighPolarity ccRst0) enableGen (clockConfig (Ppm 150)) ebNode0
  clockControl1 = callistoClockControl clock1 (unsafeFromHighPolarity ccRst1) enableGen (clockConfig (Ppm 150)) ebNode1

case_clockControlMaxBound :: Assertion
case_clockControlMaxBound = do
  let
    config = clockConfig (Ppm 100)
    dataCounts = pure (cccBufferSize config) :> Nil
    changes =
      sampleN
        (fromIntegral (cccPessimisticPeriod config))
        (callistoClockControl @_ @Fast clockGen resetGen enableGen config dataCounts)

  assertBool
    "only requests speed up"
    (SpeedUp `elem` changes && SlowDown `notElem` changes)

case_clockControlMinBound :: Assertion
case_clockControlMinBound = do
  let
    config = clockConfig (Ppm 100)
    dataCounts = pure 0 :> Nil
    changes =
      sampleN
        (fromIntegral (cccPessimisticPeriod config))
        (callistoClockControl @_ @Fast clockGen resetGen enableGen config dataCounts)

  assertBool
    "only requests slow down"
    (SlowDown `elem` changes && SpeedUp `notElem` changes)

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMaxBound :: Assertion
case_elasticBufferMaxBound = do
  let dataCounts =
        fst <$>
          sampleN 1024 (fst (elasticBuffer 32 (clockGen @Slow) (clockGen @Fast) (pure True) (pure True)))
  -- it never hits exactly the maximum because the occupancy is in the read
  -- domain, i.e. we have to look for one less than the max
  assertBool "elastic buffer should reach its near maximum (read domain)" (31 `elem` dataCounts)
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)

-- | When the elasticBuffer is written to more quickly than it is being read from,
-- its data count should reach 'maxBound'.
case_elasticBufferMinBound :: Assertion
case_elasticBufferMinBound = do
  let dataCounts =
          fst <$>
            sampleN 1024 (fst (elasticBuffer 32 (clockGen @Fast) (clockGen @Slow) (pure True) (pure True)))
  assertBool "elastic buffer should reach its minimum" (0 `elem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (32 `notElem` dataCounts)

-- | When the elasticBuffer written to as quikly to as it is read from, it should
-- reach neiher its maxBound nor minBound.
case_elasticBufferEq :: Assertion
case_elasticBufferEq = do
  let dataCounts =
          fst <$>
            sampleN 1024 (fst (elasticBuffer 32 (clockGen @Slow) (clockGen @Slow) (pure True) (pure True)))
  assertBool "elastic buffer should not reach its minimum" (0 `notElem` dataCounts)
  assertBool "elastic buffer should not reach its maximum" (32 `notElem` dataCounts)
