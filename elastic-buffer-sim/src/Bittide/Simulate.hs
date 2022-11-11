{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Simulate where

import Clash.Prelude
import Clash.Signal.Internal
import GHC.Stack

import Bittide.Simulate.Ppm
import Bittide.ClockControl

--
-- TODO:
--
--   * Reset adjustment to zero after reset assertion
--
tunableClockGen ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  -- | Settings and properties of this clock board.
  ClockConfig ->
  -- | Offset from the ideal period (encoded in the domain) of this clock. For
  -- the Si5395/Si5391 oscillators, this value lies between ±100 ppm.
  Ppm ->
  -- | Speed change request. After submitting 'SpeedUp'/'SpeedDown', caller
  -- shouldn't submit another request for 1 microsecond (i.e., the clock tuner
  -- effectively operates at 1 MHz).
  --
  -- TODO: For the actual boards this needs to be modelled as a pulse. This pulse
  -- should be asserted for at least 100 ns and at a maximum rate of 1 MHz.
  --
  Signal dom SpeedChange ->
  -- | Clock with a dynamic frequency. Note that dynamic frequencies are only
  -- relevant for components handling multiple domains. Dynamic clocks are a
  -- new, experimental feature in Clash 1.8.
  Clock dom
tunableClockGen ClockConfig{cccSettlePeriod, cccStepSize} offsetPpm speedChange =
  Clock SSymbol (Just clockSignal)
 where
  basePeriod = Femtoseconds (1000 * snatToNum (clockPeriod @dom))
  initPeriod = speedUpPeriod offsetPpm basePeriod
  clockSignal = initPeriod :- go (Femtoseconds 0) initPeriod speedChange

  go ::
    Femtoseconds -> -- settle counter
    Femtoseconds -> -- current period size
    Signal dom SpeedChange ->
    Signal dom Femtoseconds
  go !settleCounter !period (sc :- scs) =
    let
      (newSettleCounter, newPeriod) = case sc of
        SpeedUp
          | settleCounter >= cccSettlePeriod ->
              (Femtoseconds 0, speedUpPeriod cccStepSize period)
          | otherwise -> error "tunableClockGen: frequency change requested too often"
        SlowDown
          | settleCounter >= cccSettlePeriod ->
              (Femtoseconds 0, slowDownPeriod cccStepSize period)
          | otherwise -> error "tunableClockGen: frequency change requested too often"
        NoChange ->
          (fsAdd settleCounter period, period)
    in
      newPeriod :- go newSettleCounter newPeriod scs

  fsAdd :: Femtoseconds -> Femtoseconds -> Femtoseconds
  fsAdd (Femtoseconds fs0) (Femtoseconds fs1) = Femtoseconds (fs0 + fs1)

-- | Determines how 'elasticBuffer' should respond to underflow/overflow.
data OverflowMode
  -- | Saturate at empty/full boundaries. Useful for testing purposes.
  = Saturate
  -- | Error on write-when-full, or read-when-empty. This mode shuold be used in
  -- practise. Overflowing elastic buffer cannot happen in a real Bittide system.
  | Error

-- | Simple model of a FIFO that only models the interesting part for conversion:
-- data counts. Starts
elasticBuffer ::
  forall n readDom writeDom.
  (HasCallStack, KnownDomain readDom, KnownDomain writeDom, KnownNat n) =>
  -- | What behavior to pick on underflow/overflow
  OverflowMode ->
  Clock readDom ->
  Clock writeDom ->
  Signal readDom (DataCount n)
elasticBuffer mode clkRead clkWrite =
  go (clockTicks clkWrite clkRead) targetDataCount
 where
  go (tick:ticks) !fillLevel =
    case tick of
      ClockA  -> goWrite ticks fillLevel
      ClockB  -> goRead ticks fillLevel
      ClockAB -> go (ClockB:ClockA:ticks) fillLevel
  go [] _ =
    error "elasticBuffer.go: `ticks` should have been an infinite list"

  goWrite ticks fillLevel =
    go ticks newFillLevel
   where
    newFillLevel
      | fillLevel >= targetDataCount = case mode of
          Saturate -> fillLevel
          Error -> error "elasticBuffer: overflow"
      | otherwise = fillLevel + 1

  goRead ticks fillLevel =
    newFillLevel :- go ticks newFillLevel
   where
    newFillLevel
      | fillLevel <= 0 = case mode of
          Saturate -> 0
          Error -> error "elasticBuffer: underflow"
      | otherwise = fillLevel - 1
