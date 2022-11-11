module Bittide.Instances.ClockControl.Strategies.Callisto where

import Clash.Prelude (exposeClockResetEnable)
import Clash.Explicit.Prelude

import Bittide.Instances.Domains

import Bittide.ClockControl.Strategies.Callisto (callisto)
import Bittide.ClockControl
import Bittide.ClockControl.ClockGen

callisto8 ::
  Clock Basic200 ->
  Reset Basic200 ->
  Enable Basic200 ->
  Unsigned 32 ->
  Signal Basic200 (Vec 8 DataCount) ->
  Signal Basic200 SpeedChange
callisto8 = exposeClockResetEnable (callisto @8 @Basic200 128)
