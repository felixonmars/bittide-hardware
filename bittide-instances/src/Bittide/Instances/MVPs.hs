-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
module Bittide.Instances.MVPs where

import Clash.Prelude

import Bittide.Instances.Domains
import Bittide.ElasticBuffer
import Bittide.ClockControl.Callisto
import Bittide.ClockControl
import Clash.Annotations.TH (makeTopEntity)
import Bittide.ClockControl.StabilityChecker (stabilityChecker)

type FINC = Bool
type FDEC = Bool

speedChangeToPins :: SpeedChange -> (FINC, FDEC)
speedChangeToPins = \case
 SpeedUp -> (True, False)
 SlowDown -> (False, True)
 NoChange -> (False, False)

genericClockControlDemo0 ::
  forall recovered controlled  dataCountBits .
  ( KnownDomain recovered, KnownDomain controlled, KnownNat dataCountBits, 4 <= dataCountBits, dataCountBits <= 17) =>
  ClockControlConfig controlled  dataCountBits ->
  Clock recovered ->
  Clock controlled ->
  Reset controlled ->
  Reset controlled->
  Reset controlled->
  Signal controlled ((FINC, FDEC), Bool, Bool, Bool, EbMode)
genericClockControlDemo0 config clkRecovered clkControlled rstControlled drainFifo stabilityCheckReset =
  bundle (speedChangeSticky, underFlowed, overFlowed, isStable, ebMode)
 where
  speedChangeSticky =
    withClockResetEnable clkControlled rstControlled enableGen $
      stickyBits d15 (speedChangeToPins <$> speedChange)

  speedChange = callistoClockControl @1 clkControlled clockControlReset enableGen
    config (bufferOccupancy :> Nil)
  clockControlReset = unsafeFromLowPolarity $ (==Pass) <$> ebMode

  (bufferOccupancy, underFlowed, overFlowed, ebMode) =
    withReset rstControlled $
      resettableXilinxElasticBuffer clkControlled clkRecovered drainFifo

  isStable =
    withClockResetEnable clkControlled stabilityCheckReset enableGen $
      stabilityChecker d2 (SNat @20_000_000) bufferOccupancy

clockControlDemo0 ::
  "clkRecovered" ::: Clock Internal ->
  "clkControlled" ::: Clock External ->
  "rstExternal" ::: Reset External ->
  "drainFifo" ::: Reset External ->
  "stabilityCheckReset" ::: Reset External ->
  "" :::Signal External
    (
      "" :::
      ( "FINC" ::: FINC
      , "FDEC" ::: FDEC
      )
    , "Underflowed" ::: Bool
    , "Overflowed" ::: Bool
    , "isStable" ::: Bool
    , "EbMode" ::: EbMode)
clockControlDemo0 = genericClockControlDemo0 clockConfig
 where
  clockConfig :: ClockControlConfig External 12
  clockConfig = $(lift (defClockConfig @External))

-- | Holds any @a@ which has any bits set for @stickyCycles@ clock cycles.
-- On receiving a new @a@ with non-zero bits, it sets the new incoming value as it output
-- and holds it for @stickyCycles@ clock cycles.
stickyBits ::
  forall dom stickyCycles a .
  ( HiddenClockResetEnable dom
  , KnownNat (BitSize a)
  , NFDataX a
  , BitPack a
  , 1 <= stickyCycles) =>
  SNat stickyCycles ->
  Signal dom a ->
  Signal dom a
stickyBits SNat = mealy go (0 , unpack 0)
 where
  go :: (Index stickyCycles, a) -> a -> ((Index stickyCycles, a), a)
  go (count, storedBits) incomingBits = ((nextCount, nextStored), storedBits)
   where
    newIncoming = pack incomingBits /= 0
    predCount = satPred SatZero count
    holdingBits = count /= 0
    (nextStored, nextCount)
      | newIncoming = (incomingBits, maxBound)
      | holdingBits = (storedBits, predCount)
      | otherwise   = (unpack 0, predCount)

makeTopEntity 'clockControlDemo0
