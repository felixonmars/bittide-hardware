-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
module Bittide.Instances.MVPs where

import Clash.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Clash.Cores.Xilinx.Extra (ibufds, ibufds_gte3)
import qualified Clash.Explicit.Prelude as E

import Bittide.Arithmetic.Time (Nanoseconds)
import Bittide.Instances.Domains
import Bittide.ElasticBuffer
import Bittide.ClockControl.Callisto
import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker (stabilityChecker, settled)
import Bittide.Transceiver

import Bittide.ClockControl.Si539xSpi
import Bittide.ClockControl.Si5395J

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


{-# ANN testGth (Synthesize
    { t_name = "testGth"
    , t_inputs =
        [
          PortName "RX_N"
        , PortName "RX_P"
        , PortName "SYSCLK_300_N"
        , PortName "SYSCLK_300_P"
        , PortName "CPU_RESET"
        , PortName "SMA_MGT_REFCLK_C_N"
        , PortName "SMA_MGT_REFCLK_C_P"

        , PortName "GPIO_SW_E"
        , PortName "GPIO_SW_N"
        , PortName "GPIO_SW_C"
        , PortName "shared_reset_btn"
        , PortName "CLK_MISO"
        ]
    , t_output = PortProduct ""
        [ PortName "TX_N"
        , PortName "TX_P"

        , PortName "USER_SMA_GPIO_P"
        , PortName "USER_SMA_GPIO_N"


        , PortName "rx_data_good"
        , PortProduct "CLK"
          [ PortName "SCLK"
          , PortName "MOSI"
          , PortName "CSB"
          ]
        , PortName "blinkTx"
        ]
    }) #-}
testGth :: forall chansUsed . (chansUsed ~ 7)
  => "rxn_in" ::: Vec chansUsed (Signal RxS (BitVector 1))
  -> "rxp_in" ::: Vec chansUsed (Signal RxS (BitVector 1))
  -> Clock Basic300 -- "sysclk_300_n"
  -> Clock Basic300 -- "sysclk_300_p"
  -> Reset Basic300 -- "CPU_RESET"
  -> Clock Basic200 -- "sma_mgt_refclk_n"
  -> Clock Basic200 -- "sma_mgt_refclk_p"
  -> Signal Basic125 Bool
  -> Signal Basic125 Bool
  -> Signal Basic125 Bool
  -> Signal Basic125 Bool

  -> Signal Basic125 Bit

  -> ( "txn_out" ::: Vec chansUsed (Signal TxS (BitVector 1))
     , "txp_out" ::: Vec chansUsed (Signal TxS (BitVector 1))


     -- , Signal Freerun (BitVector 1)
     -- , Signal TxUser2 (BitVector 1)
     -- , Signal RxUser2 (BitVector 1)

     , Clock TxUser2
     , Clock RxUser2

     , Signal RxUser2 (Vec chansUsed Bool) -- rx_data_goods

     , ( Signal Basic125 Bool
       , Signal Basic125 Bit
       , Signal Basic125 Bool
       )
     , Signal TxUser2 (BitVector 1)
     )
testGth
  rxn_ins
  rxp_ins

  sysclk_n
  sysclk_p

  rstbtn
  -- gtrefclk0_in
  gtrefclk_n
  gtrefclk_p
  sw_e
  sw_n
  sw_c
  shared_reset_btn

  miso
   = ( txn_outs, txp_outs

     , tx_clk, rx_clk
     , rx_data_goods

     , spiOut
     , blinkTx
     )
 where
  gtrefclk = ibufds_gte3 gtrefclk_n gtrefclk_p
  (freeclk,freelocked) = clockWizardDifferential @Basic300 (SSymbol @"clk_wiz_0") sysclk_n sysclk_p rstbtn

  chanNms, refClks :: Vec chansUsed String
  chanNms = takeI @chansUsed $ "X0Y10" :> "X0Y9" :> "X0Y16" :> "X0Y17" :> "X0Y18" :> "X0Y19" :> "X0Y11" :> Nil
  refClks = takeI @chansUsed $ "clk0"  :> "clk0" :> "clk0-2":> "clk0-2":> "clk0-2":> "clk0-2":> "clk0"  :> Nil

  (txClks, rxClks, txn_outs, txp_outs, rx_data_good) = unzip5 $ transceiverPrbsN gtrefclk freeclk (sw_e .||. refClkNotReady) (sw_c .||. shared_reset_btn) sw_n chanNms refClks rxn_ins rxp_ins

  tx_clk :> _ = txClks
  rx_clk :> _ = rxClks

  rx_data_goods = bundle $ rx_data_good

  freerst = unsafeFromLowPolarity freelocked

  -- Spi core, the maximum clock period of 75 Nanoseconds leads to a nice 5 to 1 clock
  -- divider at 125MHz, resulting in a SPI clock frequencu of 12.5MHz. The
  -- SPI core _should_ be able to support a SPI clock frequency of 20MHz.
  -- EDIT running now at 1.25MHz
  (_, spiBusy, configState, spiOut) = withClockResetEnable freeclk freerst enableGen
    si539xSpi testConfig6_200_on_0a (SNat @(Nanoseconds 750)) freqOp miso
  freqOp = pure Nothing
  refClkNotReady = fmap ( /= Finished) configState

  blinkTx = blinker tx_clk noReset

noReset :: KnownDomain dom => Reset dom
noReset = unsafeFromHighPolarity (pure False)

type OneSec = 1000_000_000_000

-- blinker: 1 Hz, duty cycle 1/4
blinker :: forall dom. (KnownDomain dom, KnownNat (DomainPeriod dom), DomainPeriod dom <= OneSec, 1 <= (DomainPeriod dom), 1 <= (Div OneSec (DomainPeriod dom))) => Clock dom -> Reset dom -> Signal dom (BitVector 1)
blinker clk rst = (pack . (<= pulseWidth)) <$> cntr
  where
    cntr = E.register clk rst enableGen resetVal ((satSucc SatWrap) <$> cntr)
    resetVal = 0 :: Index (OneSec `Div` (DomainPeriod dom))
    pulseWidth = maxBound `div` 4
