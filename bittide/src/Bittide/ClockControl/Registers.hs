-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}


module Bittide.ClockControl.Registers where

import Clash.Prelude

import Protocols
import Protocols.Internal
import Protocols.Wishbone

import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Wishbone
import Clash.Functor.Extra

type StableBool = Bool
type SettledBool = Bool

-- | A wishbone accessible clock control interface.
-- This interface receives the link mask and 'DataCount's from all links.
-- Furthermore it produces FINC/FDEC pulses for the clock control boards.
--
-- The word-aligned address layout of the Wishbone interface is as follows:
--
-- - Address 0: Number of links
-- - Address 1: Link mask
-- - Address 2: FINC/FDEC
-- - Address 3: Link stables
-- - Address 4: Link settles
-- - Addresses 5 to (5 + nLinks): Data counts
clockControlWb ::
  forall dom addrW nLinks m margin framesize .
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , 2 <= addrW
  , 1 <= framesize
  , KnownNat nLinks
  , KnownNat m
  , m <= 32
  , nLinks <= 32
  ) =>
  -- | Maximum number of elements the incoming buffer occupancy is
  -- allowed to deviate from the current @target@ for it to be
  -- considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy
  -- must remain within the @margin@ for it to be considered "stable".
  SNat framesize ->
  -- | Link mask
  Signal dom (BitVector nLinks) ->
  -- | Counters
  Vec nLinks (Signal dom (DataCount m)) ->
  -- | Wishbone accessible clock control circuitry
  Circuit
    (Wishbone dom 'Standard addrW (BitVector 32))
    (CSignal dom (Bool, Bool))
clockControlWb margin framesize linkMask counters = Circuit go
 where
  go (wbM2S, _) = (wbS2M, CSignal fIncDec2)
   where
    stabilityIndications = bundle $ stabilityChecker margin framesize <$> counters
    readVec =
      pure (natToNum @nLinks)
      :> (zeroExtend @_ @_ @(32 - nLinks) <$> linkMask)
      :> (resize . pack <$> fIncDec1)
      :> (resize . pack . fmap stable <$> stabilityIndications)
      :> (resize . pack . fmap settled <$> stabilityIndications)
      :> (pack . (extend @_ @_ @(32 - m)) <<$>> counters)
    fIncDec0 = (\v -> unpack . resize <$> v !! (2 :: Unsigned 2)) <$> writeVec
    fIncDec1 = regMaybe NoChange fIncDec0
    fIncDec2 = speedChangeToFincFdec hasClock hasReset fIncDec1
    (writeVec, wbS2M) = unbundle $ wbToVec <$> bundle readVec <*> wbM2S
