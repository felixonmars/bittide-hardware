-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
module Bittide.Instances.MVPs where

import Clash.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Clash.Cores.Xilinx.Extra (ibufds)

import Bittide.Instances.Domains
import Bittide.ElasticBuffer
import Bittide.ClockControl.Callisto
import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker (stabilityChecker)


type FINC = Bool
type FDEC = Bool

speedChangeToPins :: SpeedChange -> (FINC, FDEC)
speedChangeToPins = \case
 SpeedUp -> (True, False)
 SlowDown -> (False, True)
 NoChange -> (False, False)

clockControlDemo1 ::
  "USER_SMA_CLOCK_N" ::: Clock Basic200A ->
  "USER_SMA_CLOCK_P" ::: Clock Basic200A ->
  "FMC_HPC_CLK1_M2C_N" ::: Clock Basic200B ->
  "FMC_HPC_CLK1_M2C_P" ::: Clock Basic200B ->
  "drainFifoA" ::: Reset Basic200A ->
  "drainFifoB" ::: Reset Basic200B ->
  ( "domA" ::: Signal Basic200A
    ( "" :::
      ( "FINC" ::: FINC
      , "FDEC" ::: FDEC
      )
    , "isStable" ::: Bool
    )
  ,  "domB" ::: Signal Basic200B
    ( "" :::
      ( "FINC" ::: FINC
      , "FDEC" ::: FDEC
      )
    , "isStable" ::: Bool
    )
  )
clockControlDemo1 clkSmaN clkSmaP clkFmcN clkFmcP drainFifoA drainFifoB =
  (bundle (fIncDecA, isStableA), bundle (fIncDecB, isStableB))
 where
  (fIncDecA, _, _, isStableA, _) = unbundle demoA
  (fIncDecB, _, _, isStableB, _) = unbundle demoB

  demoA =
    genericClockControlDemo0 clockConfigA clkB clkA (unsafeFromHighPolarity $ pure False)
    drainFifoA (unsafeFromHighPolarity $ pure False)

  demoB =
    genericClockControlDemo0 clockConfigB clkA clkB (unsafeFromHighPolarity $ pure False)
    drainFifoB (unsafeFromHighPolarity $ pure False)

  clkA = ibufds clkSmaP clkSmaN
  clkB = ibufds clkFmcP clkFmcN

  clockConfigA :: ClockControlConfig Basic200A 12 8 1500000
  clockConfigA = $(lift (defClockConfig @Basic200A))

  clockConfigB :: ClockControlConfig Basic200B  12 8 1500000
  clockConfigB = $(lift (defClockConfig @Basic200B))

genericClockControlDemo0 ::
  forall recovered controlled  dataCountBits margin framesize.
  ( KnownDomain recovered
  , KnownDomain controlled
  , KnownNat dataCountBits
  , KnownNat margin
  , KnownNat framesize
  , 1 <= framesize
  , 4 <= dataCountBits
  , dataCountBits <= 17) =>
  ClockControlConfig controlled  dataCountBits margin framesize ->
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
  availableLinkMask = pure $ complement 0 -- all links available
  speedChange = callistoClockControl @1 clkControlled clockControlReset enableGen
    config availableLinkMask (bufferOccupancy :> Nil)
  clockControlReset = unsafeFromLowPolarity $ (==Pass) <$> ebMode

  writeData = pure (0 :: Unsigned 8)

  (bufferOccupancy, underFlowed, overFlowed, ebMode, _) =
    withReset rstControlled $
      resettableXilinxElasticBuffer clkControlled clkRecovered drainFifo writeData

  isStable =
    withClockResetEnable clkControlled stabilityCheckReset enableGen $
      snd <$> stabilityChecker d2 (SNat @20_000_000) bufferOccupancy

clockControlDemo0 ::
  "SYSCLK_300_N" ::: Clock Basic300 ->
  "SYSCLK_300_P" ::: Clock Basic300 ->
  "USER_SMA_CLOCK_N" ::: Clock External ->
  "USER_SMA_CLOCK_P" ::: Clock External ->
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
clockControlDemo0 clkSysN clkSysP clkSmaN clkSmaP =
  genericClockControlDemo0 clockConfig clkRecovered clkControlled
 where
  clockConfig :: ClockControlConfig External 12 8 1500000
  clockConfig = $(lift (defClockConfig @External))
  (clkRecovered, _) =
    clockWizardDifferential
    @_
    @Internal
    (SSymbol @"clkWiz300to200")
    clkSysN
    clkSysP
    resetGen

  clkControlled = ibufds clkSmaP clkSmaN

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
makeTopEntity 'clockControlDemo1
