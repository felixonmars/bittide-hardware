-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}

module Bittide.ClockControl.ElasticBuffer where

import Clash.Prelude

import Clash.Cores.Xilinx.DcFifo
import Clash.Signal.Internal
import GHC.Stack

import Bittide.ClockControl

import qualified Clash.Explicit.Prelude as E

-- | Determines how 'elasticBuffer' should respond to underflow/overflow.
data OverflowMode
  -- | Saturate at empty/full boundaries. Useful for testing purposes.
  = Saturate
  -- | Error on write-when-full, or read-when-empty. This mode should be used in
  -- practice. Overflowing elastic buffer cannot happen in a real Bittide system.
  | Error

-- | Simple model of a FIFO that only models the interesting part for conversion:
-- data counts.
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

  goWrite ticks fillLevel = go ticks newFillLevel
   where
    newFillLevel
      | fillLevel == maxBound = case mode of
          Saturate -> fillLevel
          Error -> error "elasticBuffer: overflow"
      | otherwise = fillLevel + 1

  goRead ticks fillLevel = newFillLevel :- go ticks newFillLevel
   where
    newFillLevel
      | fillLevel <= 0 = case mode of
          Saturate -> 0
          Error -> error "elasticBuffer: underflow"
      | otherwise = fillLevel - 1

data EbMode
  -- | Enable read, disable write
  = Drain
  -- | Enable write, disable read
  | Fill
  -- | Enable write, enable read
  | Pass

type OverOrUnderflow = Bool

ebModeToReadWrite :: EbMode -> (Bool, Bool)
ebModeToReadWrite = \case
  Fill  -> (False, True)
  Drain -> (True, False)
  Pass  -> (True, True)

-- | Create a sticky version of a boolean signal.
sticky ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
sticky clk rst a = stickyA
 where
  stickyA = E.register clk rst enableGen False (stickyA .||. a)

-- | An elastic buffer backed by a Xilinx FIFO. It exposes all its control and
-- monitor signals in its read domain.
xilinxElasticBuffer ::
  forall n readDom writeDom.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , KnownNat n
  , 4 <= n, n <= 17
  ) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Resetting resets the 'OverOrUnderflow' signals, but not the 'DataCount'
  -- ones. Make sure to hold the reset at least 3 cycles in either clock domain.
  Reset readDom ->
  Signal readDom EbMode ->
  ( Signal readDom (DataCount n)

  -- Indicates whether the FIFO under or overflowed. This signal is sticky: it
  -- will only deassert upon reset.
  , Signal readDom OverOrUnderflow
  )
xilinxElasticBuffer clkRead clkWrite rstRead ebMode =
  (readCount, isUnderflowSticky .||. isOverflowStickySynced)
 where
  rstWrite = unsafeFromHighPolarity rstWriteBool
  rstWriteBool =
    E.dualFlipFlopSynchronizer
      clkRead clkWrite noResetWrite enableGen False (unsafeToHighPolarity rstRead)

  FifoOut{readCount, isUnderflow, isOverflow} = dcFifo
    (defConfig @n){dcOverflow=True, dcUnderflow=True}
    clkWrite noResetWrite clkRead noResetRead
    writeData readEnable

  isUnderflowSticky = sticky clkRead rstRead isUnderflow

  -- We make sure to "stickify" the signal in its original domain. The synchronizer
  -- might lose samples depending on clock configurations.
  isOverflowSticky = sticky clkWrite rstWrite isOverflow
  isOverflowStickySynced =
    E.dualFlipFlopSynchronizer
      clkWrite clkRead noResetRead enableGen False isOverflowSticky

  -- We don't reset the Xilix FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetWrite = unsafeFromHighPolarity (pure True)
  noResetRead = unsafeFromHighPolarity (pure True)

  (readEnable, writeEnable) = unbundle (ebModeToReadWrite <$> ebMode)

  writeEnableSynced =
    E.dualFlipFlopSynchronizer
      clkRead clkWrite noResetWrite enableGen False writeEnable

  writeData = mux writeEnableSynced (pure (Just (0 :: Unsigned 8))) (pure Nothing)
