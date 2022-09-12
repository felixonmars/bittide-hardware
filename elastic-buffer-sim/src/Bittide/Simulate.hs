{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module Bittide.Simulate where

import Clash.Cores.Xilinx.DcFifo hiding (DataCount)
import Clash.Explicit.Prelude
import Clash.Signal.Internal
import GHC.Stack
import Numeric.Natural

import Bittide.Simulate.Ppm
import Bittide.ClockControl

-- Number of type aliases for documentation purposes in various functions defined
-- down below.
type StepSize = Natural
type InitialPeriod = Natural
type Offset = Integer
type DynamicRange = Natural

--
-- TODO:
--
--   * Reset adjustment to zero after reset assertion
--
tunableClockGen ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  -- | Period it takes for a clock frequency request to settle. This is not
  -- modelled, but an error is thrown if a request is submitted more often than
  -- this.
  SettlePeriod ->
  -- | Offset from the ideal period (encoded in the domain) of thiss clock. For
  -- the Si5395/Si5391 oscillators, this value lies between ±100 ppm.
  Offset ->
  -- | The size of the clock frequency should "jump" on a speed change request.
  StepSize ->
  -- | When asserted, clock multiplier resets the outgoing clock to its original
  -- frequency. TODO: Implement.
  Reset dom ->
  -- | Speed change request. After submitting 'SpeedUp'/'SpeedDown', caller
  -- shouldn't submit another request for 1 microsecond (i.e., the clock tuner
  -- effectively operates at 1 MHz).
  --
  -- TODO: For the actual boards this needs to be modelled as a pulse. This pulse
  -- should be asserted for at least 100 ns and at a maximum rate of 1 MHz.
  --
  Signal dom SpeedChange ->
  -- | Clock with a dynamic frequency. At the time of writing, Clash primitives
  -- don't account for this yet, so be careful when using them. Note that dynamic
  -- frequencies are only relevant for components handling multiple domains.
  Clock dom
tunableClockGen settlePeriod periodOffset stepSize _reset speedChange =
  let period = snatToNum (clockPeriod @dom)
      initPeriod = fromIntegral (period + periodOffset)
      clockSignal = initPeriod :- go settlePeriod initPeriod speedChange in
  Clock SSymbol (Just clockSignal)
 where
  go ::
    SettlePeriod ->
    PeriodPs ->
    Signal dom SpeedChange ->
    Signal dom StepSize
  go !settleCounter !period (sc :- scs) =
    let
      (newSettleCounter, newPeriod) = case sc of
        SpeedUp
          | settleCounter >= settlePeriod -> (0, period - stepSize)
          | otherwise -> error "tunableClockGen: frequency change requested too often"
        SlowDown
          | settleCounter >= settlePeriod -> (0, period + stepSize)
          | otherwise -> error "tunableClockGen: frequency change requested too often"
        NoChange ->
          (settleCounter + period, period)
    in
      newPeriod :- go newSettleCounter newPeriod scs

-- | Determines how 'elasticBuffer' should respond to underflow/overflow.
data OverflowMode
  -- | Saturate at empty/full boundaries. Useful for testing purposes.
  = Saturate
  -- | Error on write-when-full, or read-when-empty. This mode shuold be used in
  -- practise. Overflowing elastic buffer cannot happen in a real Bittide system.
  | Error

-- | Simple model of a FIFO that only models the interesting part for conversion:
-- data counts.
elasticBuffer ::
  forall readDom writeDom.
  (HasCallStack, KnownDomain readDom, KnownDomain writeDom) =>
  -- | What behavior to pick on underflow/overflow
  OverflowMode ->
  -- | Size of FIFO. To reflect our target platforms, this should be a power of two
  -- where typical sizes would probably be: 16, 32, 64, 128.
  ElasticBufferSize ->
  Clock readDom ->
  Clock writeDom ->
  Signal readDom DataCount
elasticBuffer _mode _size clkRead clkWrite =
  fromIntegral <$> readCount
 where
  waitMidway :: Signal readDom (Unsigned 12) -> Signal readDom Bool
  waitMidway = mealy clkRead resetGen enableGen go False
   where
    go True _ = (True, True)
    go False i | i >= (maxBound `div` 2) = (True, True)
               | otherwise = (False, False)
  FifoOut{..} = dcFifo (defConfig @12) clkWrite resetGen clkRead resetGen (pure (Just ())) (waitMidway readCount)
