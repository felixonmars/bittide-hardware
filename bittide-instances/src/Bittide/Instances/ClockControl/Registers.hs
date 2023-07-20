-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
module Bittide.Instances.ClockControl.Registers where

import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Registers
import Bittide.ClockControl.Si539xSpi
import Protocols.Wishbone
import Bittide.ClockControl.StabilityChecker
import Bittide.Counter
import Language.Haskell.TH
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols.Internal
import Bittide.SharedTypes
import qualified Bittide.ClockControl.Si5395J as Si5395J

import System.Directory
import System.FilePath


data TestState = Busy | Fail | Success
  deriving (Generic, NFDataX)

clockControlRegistersEntity ::
  Clock Basic200A ->
  Reset Basic200A ->
  Clock Basic200B ->
  "MISO" ::: Signal Basic200A Bit -> -- SPI
  "" :::
    ( Signal Basic200A TestState

    -- Freq increase / freq decrease request to clock board
    , ( "FINC"    ::: Signal Basic200A Bool
      , "FDEC"    ::: Signal Basic200A Bool
      )

      -- SPI to clock board:
    , "" :::
      ( "SCLK" ::: Signal Basic200A Bool
      , "MOSI" ::: Signal Basic200A Bit
      , "CSB"  ::: Signal Basic200A Bool
      )

      -- Debug signals:
    , "" :::
      ( "SPI_BUSY" ::: Signal Basic200A Bool
      , "SPI_STATE" ::: Signal Basic200A (BitVector 40)
      , "SI_LOCKED" ::: Signal Basic200A Bool
      , "COUNTER_ACTIVE" ::: Signal Basic200A Bool
      , "COUNTER_SETTLED" ::: Signal Basic200A Bool
      )
    )
clockControlRegistersEntity clk rst clkControlled miso =
  (testResult, unbundle fIncDec, spiOut, debugSignals)
 where
  debugSignals = (spiBusy, pack <$> spiState, siClkLocked, counterActive, counterSettled)

  (_, spiBusy, spiState@(fmap (==Finished) -> siClkLocked), spiOut) =
    withClockResetEnable clk rst enableGen $
      si539xSpi
        Si5395J.testConfig6_200_on_0a_and_0
        (SNat @(Microseconds 1))
        (pure Nothing)
        miso

  testResult = regEn clk rst enableGen Busy (watchDog .==. pure maxBound) $ mux counterSettled (pure Success) (pure Fail)
  watchDog = register clk rst enableGen (0 :: Index (PeriodToCycles Basic200 (Seconds 1))) $ satSucc SatBound <$> watchDog
  rstTest = unsafeFromActiveLow siClkLocked

  rstControlled =
    unsafeFromActiveLow $
      xpmCdcSingle clk clkControlled $ -- improvised reset syncer
        unsafeToActiveLow rst

  (fmap ((0-) . resize) -> counter, counterActive) =
    unbundle $
      -- Note that in a "real" Bittide system the clocks would be wired up the
      -- other way around: the controlled domain would be the target domain. We
      -- don't do that here because we know 'rstControlled' will come out of
      -- reset much earlier than 'rstTest'. Doing it the "proper" way would
      -- therefore introduce extra complexity, without adding to the test's
      -- coverage.
      domainDiffCounter clkControlled rstControlled clk rstTest
  counterSettled = settled <$> withClockResetEnable clk rstTest enableGen
    stabilityChecker margin framesize counter

  (_, CSignal fIncDec) = toSignals
    ( circuit $ \ unit -> do
      [wbA, wbB] <- (withClockResetEnable clk rstTest enableGen $ processingElement @Basic200A peConfig) -< unit
      fIncDecCallisto -< wbA
      withClockResetEnable clk rstTest enableGen $ clockControlWb margin framesize (pure $ complement 0) (counter :> Nil) -< wbB
    ) (() ,CSignal $ pure ())

  fIncDecCallisto ::
    (KnownNat aw, 2 <= aw) =>
    Circuit
      (Wishbone Basic200A 'Standard aw (Bytes 4))
      ()
  fIncDecCallisto = Circuit goFIncDecCallisto
   where
    goFIncDecCallisto (wbM2S, _) = (wbS2M, ())
     where
      (_, wbS2M) = withClockResetEnable clk rstTest enableGen
        $ registerWb CircuitPriority NoChange wbM2S (maybeSpeedChange <$> callistoResult)

  callistoResult = callistoClockControl clk
    rstTest enableGen clockConfig (pure maxBound)
    (counter :> Nil)

  -- Configuration for Callisto
  clockConfig :: ClockControlConfig Basic200A 31 8 1500000
  clockConfig = $(lift ((defClockConfig @Basic200A){cccPessimisticSettleCycles = 20000, cccBufferSize = d31} ))
  margin = d2
  framesize = SNat @1_000_000
  (   (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do

    let
      findProjectRoot :: IO FilePath
      findProjectRoot = goUp =<< getCurrentDirectory
        where
          goUp :: FilePath -> IO FilePath
          goUp path
            | isDrive path = error "Could not find 'cabal.project'"
            | otherwise = do
                exists <- doesFileExist (path </> projectFilename)
                if exists then
                  return path
                else
                  goUp (takeDirectory path)

          projectFilename = "cabal.project"

    root <- runIO findProjectRoot

    let elfPath = root </> "firmware/examples/target/riscv32imc-unknown-none-elf/release/clock-control"

    memBlobsFromElf BigEndian elfPath Nothing)

  {-
    0b001xxxxx_xxxxxxxx 0b0010 0x2x instruction memory
    0b010xxxxx_xxxxxxxx 0b0100 0x4x data memory
    0b100xxxxx_xxxxxxxx 0b1000 0x8x callisto register
    0b101xxxxx_xxxxxxxx 0b1010 0xAx clock control

  -}
  peConfig = PeConfig (0b001 :> 0b010 :> 0b100 :> 0b101 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)
