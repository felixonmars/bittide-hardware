{-|

Provides a rudimentary simulation of elastic buffers.

-}

-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module Bittide.Simulate where

import Clash.Explicit.Prelude
import Clash.Signal.Internal
import Data.Bifunctor (first, second)
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

type ResetClockControl = Bool -- request a reset in other elastic buffers
type HasUnderflowed = Bool
type HasOverflowed = Bool
type DisableTilHalf = Bool
type ForceReset = Bool -- force a "reset" to EB midpoint
type PropagateReset = Bool

type DisableWrites = Bool
type DisableReads = Bool

data WrResetDomain = Wait -- ^ after reset/when nothing has overflowed
                   | DisableWritesEnableReads
                   | EnableWritesDisableReads
                   deriving (Generic, NFDataX)

-- | This wrapper disables reads or writes when the elastic buffer
-- over-/underflows, as appropriate.
--
-- It also censors 'DataCount' after an over-/underflow, so that we do not take
-- measurements from an elastic buffer until it has settled back to its
-- midpoint.
ebController ::
  forall readDom writeDom.
  (KnownDomain readDom, KnownDomain writeDom) =>
  ElasticBufferSize ->
  Clock readDom ->
  Reset readDom ->
  Enable readDom ->
  Clock writeDom ->
  Reset writeDom ->
  Enable writeDom ->
  (Signal readDom DataCount, Signal readDom ResetClockControl)
ebController size clkRead rstRead enaRead clkWrite rstWrite enaWrite =
  unbundle
    (go <$> rdToggle <*> outRd <*> overflowRd <*> requestRst)
 where
  rstReadS = unsafeToHighPolarity rstRead

  -- tie off reset to mealy while we do our "reset" dance
  ne'erRst = unsafeFromHighPolarity (pure False)

  (outRd, outWr) = elasticBuffer size clkRead clkWrite rdToggle wrToggle

  go True (dc, False) False _ = (dc, False)
  go _ (dc, _) _ True = (deepErrorX "Resetting...", True)
  go _ (dc, _) _ _ = (dc, False)

  overflowRd =
    dualFlipFlopSynchronizer clkWrite clkRead rstRead enaRead False (snd <$> outWr)

  wrToggle = not <$> wrDisable; rdToggle = not <$> rdDisable

  wrDisable =
    dualFlipFlopSynchronizer clkRead clkWrite rstWrite enaWrite False wrDisableRd

  (requestRst, wrDisableRd, rdDisable) = unbundle direct

  -- Write reset process (that is accurate in the read domain):
  --
  -- 1. Disable writes (keep reads enabled), drain completely (control from the
  -- read domain)
  -- domain)
  -- 2. Re-enable writes, disable reads until data count is exactly half (in the
  -- read domain)
  -- 3. Proceed reading

  direct :: Signal readDom (PropagateReset, DisableWrites, DisableReads)
  direct =
    register clkRead ne'erRst enaRead (False, False, False)
      $ mealy clkRead ne'erRst enaRead f Wait (bundle (rstReadS, outRd, overflowRd))
   where
    f ::
      WrResetDomain ->
      (ForceReset, (DataCount, Underflow), Overflow) ->
      (WrResetDomain, (PropagateReset, DisableWrites, DisableReads))
    f _ (True, _, _) = (DisableWritesEnableReads, (False, True, False))
    f EnableWritesDisableReads (_, (d, _), _) | d == targetDataCount size = (Wait, (False, False, False))
    f EnableWritesDisableReads _ = (EnableWritesDisableReads, (False, False, True))
    f DisableWritesEnableReads (_, (0, _), _) = (EnableWritesDisableReads, (False, False, True))
    f DisableWritesEnableReads _ = (DisableWritesEnableReads, (False, True, False))
    f Wait (_, (_, True), _) = (EnableWritesDisableReads, (True, False, True))
    f Wait (_, _, True) = (DisableWritesEnableReads, (True, True, False))
    f Wait _ = (Wait, (False, False, False))

type Underflow = Bool
type Overflow = Bool

-- | Model FIFO. This is exposed as 'ebController'
--
-- Output signal exposes 'DataCount' and over-/underflow.
elasticBuffer ::
  forall readDom writeDom.
  (KnownDomain readDom, KnownDomain writeDom) =>
  -- | Size of FIFO. To reflect our target platforms, this should be a power of two
  -- where typical sizes would probably be: 16, 32, 64, 128.
  ElasticBufferSize ->
  Clock readDom ->
  Clock writeDom ->
  -- | Read enable
  Signal readDom Bool ->
  -- | Write enable
  Signal writeDom Bool ->
  (Signal readDom (DataCount, Underflow), Signal writeDom (DataCount, Overflow))
elasticBuffer size clkRead clkWrite readEna writeEna
  | Clock _ (Just readPeriods) <- clkRead
  , Clock _ (Just writePeriods) <- clkWrite
  = go 0 (targetDataCount size) readPeriods writePeriods readEna writeEna
 where
  go !relativeTime !fillLevel rps wps@(writePeriod :- _) =
    if relativeTime < toInteger writePeriod
      then goRead relativeTime fillLevel rps wps
      else goWrite relativeTime fillLevel rps wps

  goWrite relativeTime fillLevel rps (writePeriod :- wps) rdEna (wrEna :- wrEnas)
    | wrEna =
    second (next :-) $
      go (relativeTime - toInteger writePeriod) newFillLevel rps wps rdEna wrEnas
   where
    next@(newFillLevel, _)
      | fillLevel >= size = (size, True)
      | otherwise = (fillLevel + 1, False)

  goWrite relativeTime fillLevel rps (writePeriod :- wps) rdEna (_ :- wrEnas) =
    second ((fillLevel, False) :-) $
      go (relativeTime - toInteger writePeriod) fillLevel rps wps rdEna wrEnas

  goRead relativeTime fillLevel (readPeriod :- rps) wps (rdEna :- rdEnas) wrEnas
    | rdEna =
    first (next :-) $
      go (relativeTime + toInteger readPeriod) newFillLevel rps wps rdEnas wrEnas
   where
    next@(newFillLevel, _)
      | fillLevel <= 0 = (0, True)
      | otherwise = (fillLevel - 1, False)

  goRead relativeTime fillLevel (readPeriod :- rps) wps (_ :- rdEnas) wrEnas =
    first ((fillLevel, False) :-) $
      go (relativeTime + toInteger readPeriod) fillLevel rps wps rdEnas wrEnas

elasticBuffer size (Clock ss Nothing) clock1 readEna writeEna =
  -- Convert read clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @readDom)
  in
    elasticBuffer size (Clock ss (Just (pure period))) clock1 readEna writeEna

elasticBuffer size clock0 (Clock ss Nothing) readEna writeEna =
  -- Convert write clock to a "dynamic" clock if it isn't one
  let
    period = snatToNum (clockPeriod @writeDom)
  in
    elasticBuffer size clock0 (Clock ss (Just (pure period))) readEna writeEna
