-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Instances.Calendar where

import Clash.Prelude

import Bittide.Calendar (mkCalendar, CalendarConfig (CalendarConfig))
import Bittide.Instances.Domains (Basic200)
import Bittide.SharedTypes
import Bittide.Switch
import Protocols.Wishbone

import Bittide.Instances.Hacks (reducePins)

type WishboneWidth = 4
type WishboneAddrWidth = 32


switchCalendar1k ::
  Clock Basic200 -> Reset Basic200 ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  ( Signal Basic200 (Vec 15 (CrossbarIndex 15))
  , Signal Basic200 Bool, Signal Basic200 (WishboneS2M (Bytes WishboneWidth))
  )
switchCalendar1k clk rst =
  withClockResetEnable clk syncRst enableGen $
    mkCalendar (CalendarConfig (SNat @1024) (repeat 0 :> Nil) (repeat 0 :> Nil))
 where
  syncRst = resetSynchronizer clk rst
{-# NOINLINE switchCalendar1k #-}

switchCalendar1kReducedPins ::
  Clock Basic200 -> Reset Basic200 ->
  Signal Basic200 Bit ->
  Signal Basic200 Bit
switchCalendar1kReducedPins clk rst =
  withClock clk $
    reducePins (bundle . switchCalendar1k clk rst)
{-# NOINLINE switchCalendar1kReducedPins #-}