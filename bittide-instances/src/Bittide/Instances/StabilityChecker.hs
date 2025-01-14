-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Bittide.Instances.StabilityChecker where

import Clash.Prelude

import Bittide.ClockControl.StabilityChecker
import Bittide.Instances.Domains (Basic200)
import Bittide.ClockControl (DataCount)

stabilityChecker_3_1M ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (DataCount 16) ->
  Signal Basic200 StabilityIndication
stabilityChecker_3_1M clk rst =
  withClockResetEnable clk rst enableGen $
    stabilityChecker d3 (SNat @1_000_000)
