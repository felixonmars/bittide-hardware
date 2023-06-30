-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=100 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Bittide.Instances.MVPs where

import Clash.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Xilinx.ClockGen
import Clash.Cores.Xilinx.Extra (ibufds)

-- import Clash.Signal.Internal
-- import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
-- import Bittide.ClockControl.Si5395J
-- import Bittide.ClockControl.Si539xSpi
import Bittide.ClockControl.StabilityChecker (stabilityChecker, settled)
import Bittide.Counter
import Bittide.ElasticBuffer
import Bittide.Instances.Domains
import Bittide.Transceiver
import Clash.Cores.Extra
import Clash.Cores.Xilinx.GTH
import Clash.Explicit.Reset.Extra
import Clash.Cores.Xilinx.VIO

import Clash.Cores.Xilinx.Xpm.Cdc.Single

import qualified Clash.Explicit.Prelude as E

type FINC = Bool
type FDEC = Bool

-- deriving instance
--   ( KnownDomain dom
--   , KnownNat entries
--   , 1 <= entries
--   , KnownNat (Clash.Signal.Internal.DomainConfigurationPeriod (KnownConf dom))
--   ) => BitPack (ConfigState dom entries)

clockControlDemo2 ::
  "SMA_MGT_REFCLK_C_P" ::: Clock Basic200 ->
  "SMA_MGT_REFCLK_C_N" ::: Clock Basic200 ->
  "SYSCLK_300_P" ::: Clock Basic300 ->
  "SYSCLK_300_N" ::: Clock Basic300 ->
  "RST_GLOBAL" ::: Signal GthTx Bool ->
  "RST_LOCAL" ::: Signal Basic125 Bool ->
  "RST_GTH" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: Vec 7 (Signal GthRx (BitVector 1)) ->
  "GTH_RX_PS" ::: Vec 7 (Signal GthRx (BitVector 1)) ->
  ( "GTH_TX_NS" ::: Vec 7 (Signal GthTx (BitVector 1))
  , "GTH_TX_PS" ::: Vec 7 (Signal GthTx (BitVector 1))
  , "" ::: Signal GthTx ("FINC" ::: FINC, "FDEC" ::: FDEC)
  , "Linkstatusses" ::: Vec 7 (Signal GthRx Bool)
  )
clockControlDemo2
  refClkP refClkN
  sysClkP sysClkN
  asyncRstGlobal (fmap not -> asyncRstLocal) (fmap not -> asyncRstGth)
  rxns rxps
  = hwSeqX probegthTx
    (txns, txps, frequencyAdjusters, linkUpsRx)
 where
  availableLinkMask = pure $ complement 0 -- all links available

  clockConfig = $(lift (defClockConfig @GthTx){cccBufferSize = d25})

  -- Clock wizardry
  refClk = ibufds_gte3 refClkN refClkP :: Clock Basic200

  -- clk300 :: Clock Basic300
  -- clk300 = ibufds sysClkP sysClkN

  sysClk :: Clock Basic125
  (sysClk, sysLock0) = --clockWizard (SSymbol @"SysClk") clk300 noReset
    clockWizardDifferential (SSymbol @"SysClk") sysClkN sysClkP noReset

  sysLock1 = xpmCdcSingle sysClk sysClk sysLock0

  sysRst = E.holdReset sysClk enableGen d5
    $ orReset rstLocal
    (unsafeFromLowPolarity $ (sysLock1 .&&. fmap not spiErr))

  withTxDom = withClockResetEnable txClock clockControlReset enableGen

  clockControlReset = orReset rstGlobal (unsafeFromLowPolarity $ fold (.&&.) linkUpsTx)

  gthAllReset = E.holdReset sysClk enableGen d1024 $ orReset
    (unsafeFromLowPolarity $ safeDffSynchronizer sysClk sysClk False spiDone) gthReset
  gthChanReset = E.holdReset sysClk enableGen d1024 gthAllReset
  gthStimReset = E.holdReset sysClk enableGen d1024 gthChanReset

  -- Clock programming
  spiDone = E.dflipflop sysClk $ unsafeToLowPolarity sysRst
  spiErr = E.dflipflop sysClk $ pure False

  -- isErr (Error _) = True
  -- isErr _         = False

  -- (_, _, spiState, spiOut) = withClockResetEnable sysClk sysRst enableGen $
  --   si539xSpi testConfig6_200_on_0a (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Reset filtering and synchronization
  rstLocal, gthReset :: Reset Basic125
  rstGlobal = resetGlitchFilter (SNat @100_000) txClock $ resetSynchronizer txClock
    $ unsafeFromLowPolarity asyncRstGlobal

  rstLocal = resetGlitchFilter (SNat @100_000) sysClk $ resetSynchronizer sysClk
    $ unsafeFromLowPolarity asyncRstLocal

  gthReset = resetGlitchFilter (SNat @100_000) sysClk $ resetSynchronizer sysClk
    $ unsafeFromLowPolarity asyncRstGth

  -- Use the first tx clock as logic clock
  chanNms = "X0Y10":> "X0Y9":> "X0Y16" :> "X0Y17" :> "X0Y18" :> "X0Y19" :> "X0Y11" :> Nil
  clkPaths ="clk0" :> "clk0":> "clk0-2":> "clk0-2":> "clk0-2":> "clk0-2":> "clk0"  :> Nil

  (head -> txClock, rxClocks, txns, txps, linkUpsRx) = unzip5
    $ transceiverPrbsN refClk sysClk gthAllReset gthStimReset gthChanReset
      chanNms clkPaths rxns rxps

  -- Synchronize all link status signals to the tx domain and AND them to obtain linkUpsTx
  syncLinkStatus rxClock linkStatus = safeDffSynchronizer rxClock txClock False linkStatus
  linkUpsTx = syncLinkStatus <$> rxClocks <*> linkUpsRx

  (speedChange0, stabilities, allStable0, allCentered0) = unbundle $
    fmap (\CallistoResult{..} -> (speedChange, stability, allStable, allSettled))
    $ callistoClockControl @7 @25 @GthTx txClock clockControlReset enableGen
      clockConfig availableLinkMask $ fmap (fmap resize) domainDiffs

  frequencyAdjusters = withTxDom $ stickyBits @GthTx d20 (speedChangeToPins <$> speedChange0)
  probegthTx :: Signal GthTx ()
  probegthTx =
    vioProbe
    ( "domainActives"
    :> "domainDiffs"
    :> "stabilities"
    :> "allStable"
    :> "allCentered"
    :> Nil)
    Nil
    ()
    txClock
    (E.dflipflop txClock $ pack <$> bundle domainActives)
    (E.dflipflop txClock $ pack <$> bundle domainDiffs)
    (E.dflipflop txClock $ pack <$> stabilities)
    (E.dflipflop txClock $ pack <$> allStable0)
    (E.dflipflop txClock $ pack <$> allCentered0)

  (domainDiffs, domainActives) = unzip $ fmap unbundle $ rxDiffCounter <$> rxClocks <*> linkUpsRx
  rxDiffCounter rxClk linkUp =
    domainDiffCounter rxClk (unsafeFromLowPolarity linkUp) txClock clockControlReset

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
      stickyBits d15 (speedChangeToPins . speedChange <$> callistoResult)
  availableLinkMask = pure $ complement 0 -- all links available
  callistoResult =
    callistoClockControl @1 clkControlled clockControlReset enableGen
      config availableLinkMask (bufferOccupancy :> Nil)
  clockControlReset = unsafeFromLowPolarity $ (==Pass) <$> ebMode

  writeData = pure (0 :: DataCount 8)

  (bufferOccupancy, underFlowed, overFlowed, ebMode, _) =
    withReset rstControlled $
      resettableXilinxElasticBuffer clkControlled clkRecovered drainFifo writeData

  isStable =
    withClockResetEnable clkControlled stabilityCheckReset enableGen $
      settled <$> stabilityChecker d2 (SNat @20_000_000) bufferOccupancy

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
makeTopEntity 'clockControlDemo2
