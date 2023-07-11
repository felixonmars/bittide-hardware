-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Registers where

import Clash.Prelude
import Protocols
import Protocols.Internal (CSignal)
import Protocols.Wishbone


type StableBool = Bool
type SettledBool = Bool


-- |
mm ::
  ( HiddenClockResetEnable dom
  , KnownNat regs
  , KnownNat addrW
  , 2 <= addrW
  ) =>
  -- | Counters
  Signal dom (Vec n Int) ->
  -- | All stable
  Signal dom StableBool ->
  -- | All stable and centered
  Signal dom SettledBool ->
  -- | Reset speedchange register (value has been read)
  Signal dom (SpeedChangeAck) ->
  Circuit
    (Wishbone dom 'Standard addrW (BitVector 32))
    (CSignal dom SpeedChange)
mm counters stable settled speedChangeAck = Circuit go
 where
  go ::
    (Signal dom (WishboneM2S addrW regs (BitVector 32)), CSignal dom ()) ->
    (Signal dom (WishboneS2M (BitVector 32)), CSignal dom SpeedChange)
  go master@(WishBoneM2S{..}) = bundle (wbS2M, speedChangeReg)
   where
    speedChangeReg =
      withReset (unsafeFromHighPolarity speedChangeAck) $
      register speedChange
    wbS2M
      -- not in cycle
      | not (busCycle && strobe)
      = emptyWishboneS2M
      -- illegal addr
      | not addrLegal
      = emptyWishboneS2M @(BitVector 32) {err = True, readData = invalidReq, Ack False, ()}
      -- read at 0
      | not writeEnable && internalAddr == 0
      = emptyWishboneS2M @(BitVector 32) {readData, acknowledge}
     where
      (alignedAddr, alignment) = split @_ @(addrW - 2) addr
      internalAddr = bitCoerce $ resize alignedAddr :: Index regs
      addrLegal = alignedAddr <= (natToNum @regs) && alignment == 0
