-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Clock controller types and some constants/defaults.
module Bittide.ClockControl
  ( ClockConfig (..)
  , DataCount
  , SpeedChange (..)
  , defClockConfig
  , targetDataCount
  , pessimisticSettleCycles
  )
where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Femtoseconds(..))
import Data.Aeson (ToJSON(toJSON))
import Data.Csv
import Data.Proxy (Proxy(..))
import GHC.Stack (HasCallStack)

import Bittide.Simulate.Time (microseconds)
import Bittide.Simulate.Ppm (Ppm(..), speedUpPeriod)

type DataCount n = Unsigned n

-- | A combination of properties and settings of/for the oscillator and PLL.
--
data ClockConfig = ClockConfig
  { -- | Period it takes for a clock frequency request to settle. This is not
    -- modelled, but an error is thrown if a request is submitted more often than
    -- this. 'clockControl' should therefore not request changes more often.
    --
    -- This is a PLL property.
    --
    cccSettlePeriod :: Femtoseconds

    -- | Maximum deviation from "factory tuning". E.g., a clock tuned to 200 MHz
    -- and a maximum deviation of +- 100 ppm can produce a signal anywhere
    -- between 200 MHz +- 20 KHz.
    --
    -- This is an oscillator + PLL property.
    --
  , cccDeviation :: Ppm

    -- | Step size for frequency increments / decrements
    --
    -- This is a setting of the PLL. Note that though this is a setting, it is
    -- programmed over I2C. For the time being, we expect it to be programmed
    -- once after which only the FDEC/FINC pins will be used.
    --
  , cccStepSize :: Ppm

  } deriving (Lift)

-- | Calculate target data count given a FIFO size. Currently returns a target
-- data count of half the FIFO size.
targetDataCount :: KnownNat n => DataCount n
targetDataCount = shiftR maxBound 1

-- | Safer version of FINC/FDEC signals present on the Si5395/Si5391 clock multipliers.
data SpeedChange
  = SpeedUp
  | SlowDown
  | NoChange
  deriving (Eq, Show, Generic, ShowX, NFDataX)

instance ToField SpeedChange where
  toField SpeedUp = "speedUp"
  toField SlowDown = "slowDown"
  toField NoChange = "noChange"

instance KnownNat n => ToField (Unsigned n) where
  toField = toField . toInteger

instance KnownNat n => ToJSON (Unsigned n) where
  toJSON = toJSON . toInteger

-- | Clock settings inspired by the Si5395/Si5391 clock multiplier boards.
defClockConfig :: ClockConfig
defClockConfig = ClockConfig
  { cccSettlePeriod = microseconds 1
  , cccDeviation    = Ppm 100
  , cccStepSize     = Ppm 1
  }

-- | Number of cycles to wait on a given clock frequency and clock settings in
-- order for the settle period to pass. /Pessimistic/ means that it calculates
-- this for the fastest possible clock.
--
pessimisticSettleCycles ::
  forall dom.
  ( HasCallStack
  , KnownDomain dom ) =>
  Proxy dom ->
  ClockConfig ->
  -- | It would take a 10 GHz clock only a 10_000 cycles to wait 1 µs. This can be
  -- met by an @Unsigned 14@: @2^14 ~ 16384@. We round up to the nearest "sensible"
  -- power, 16. Even so, an error is thrown if the result does not fit.
  Unsigned 16
pessimisticSettleCycles Proxy ClockConfig{cccSettlePeriod, cccDeviation} =
  checkedFromIntegral nCycles
 where
  nCycles = (settlePeriod `div` pessimisticPeriod) + 1
  Femtoseconds settlePeriod = cccSettlePeriod
  period = Femtoseconds (1000 * snatToNum (clockPeriod @dom))
  Femtoseconds pessimisticPeriod = speedUpPeriod cccDeviation period
