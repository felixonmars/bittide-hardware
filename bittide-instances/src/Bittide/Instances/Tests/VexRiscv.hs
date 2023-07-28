-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Tests.VexRiscv where

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Ila (ila, ilaConfig)

import Clash.Prelude

import Bittide.Instances.Domains (Basic200, Basic125)
import Bittide.DoubleBufferedRam
import Language.Haskell.TH (runIO)
import System.Directory
import System.FilePath
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes

import Protocols
import Protocols.Internal
import Protocols.Wishbone
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Clash.Reset.Extra (noReset, orReset)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)

data TestStatus = Running | Success | Fail
  deriving (Enum, Eq, Generic, NFDataX, BitPack)

vexRiscvInner ::
  forall dom.
  HiddenClockResetEnable dom =>
  Signal dom ((Bool, Bool), WishboneM2S 32 4 (BitVector 32))
vexRiscvInner = hwSeqX dbgIla $ bundle (stateToDoneSuccess <$> status, iBus)
  where

    dbgIla :: Signal dom ()
    dbgIla =
      ila
        (ilaConfig $
             "idbg_ibus_addr"
          :> "idbg_ibus_writeData"
          :> "idbg_ibus_busSelect"
          :> "idbg_ibus_busCycle"
          :> "idbg_ibus_strobe"
          :> "idbg_ibus_writeEnable"
          :> "idbg_ibus_cycleTypeIdentifier"
          :> "idbg_ibus_burstTypeExtension"

          :> "idbg_dbus_addr"
          :> "idbg_dbus_writeData"
          :> "idbg_dbus_busSelect"
          :> "idbg_dbus_busCycle"
          :> "idbg_dbus_strobe"
          :> "idbg_dbus_writeEnable"
          :> "idbg_dbus_cycleTypeIdentifier"
          :> "idbg_dbus_burstTypeExtension"

          :> "idbg_dbus_status_addr"
          :> "idbg_dbus_status_writeData"
          :> "idbg_dbus_status_busSelect"
          :> "idbg_dbus_status_busCycle"
          :> "idbg_dbus_status_strobe"
          :> "idbg_dbus_status_writeEnable"
          :> "idbg_dbus_status_cycleTypeIdentifier"
          :> "idbg_dbus_status_burstTypeExtension"

          :> "idbg_status"
          :> Nil
        )
        hasClock
        (addr                 <$> iBus)
        (writeData            <$> iBus)
        (busSelect            <$> iBus)
        (busCycle             <$> iBus)
        (strobe               <$> iBus)
        (writeEnable          <$> iBus)
        (cycleTypeIdentifier  <$> iBus)
        (burstTypeExtension   <$> iBus)

        (addr                 <$> dBus)
        (writeData            <$> dBus)
        (busSelect            <$> dBus)
        (busCycle             <$> dBus)
        (strobe               <$> dBus)
        (writeEnable          <$> dBus)
        (cycleTypeIdentifier  <$> dBus)
        (burstTypeExtension   <$> dBus)

        (addr                 <$> dBusStatus)
        (writeData            <$> dBusStatus)
        (busSelect            <$> dBusStatus)
        (busCycle             <$> dBusStatus)
        (strobe               <$> dBusStatus)
        (writeEnable          <$> dBusStatus)
        (cycleTypeIdentifier  <$> dBusStatus)
        (burstTypeExtension   <$> dBusStatus)

        (pack <$> status)


    stateToDoneSuccess Running = (False, False)
    stateToDoneSuccess Success = (True, True)
    stateToDoneSuccess Fail    = (True, False)

    unitC = CSignal (pure ())
    (_, (CSignal status, CSignal iBus, CSignal dBus, CSignal dBusStatus)) = circuitFn ((), (unitC, unitC, unitC, unitC))

    Circuit circuitFn = circuit $ \unit -> do
        ([wb], iBusCopy, dBusCopy) <- processingElementDbg peConfig -< unit
        (wb1, wbCopy) <- observeWbMaster -< wb
        status <- statusRegister -< wb1
        idC -< (status, iBusCopy, dBusCopy , wbCopy)

    statusRegister :: Circuit (Wishbone dom 'Standard 30 (Bytes 4)) (CSignal dom TestStatus)
    statusRegister = Circuit $ \(fwd, CSignal _) ->
        let (unbundle -> (m2s, st)) = mealy go Running fwd
        in (m2s, CSignal st)
      where
        go st WishboneM2S{..}
          -- out of cycle, no response, same state
          | not (busCycle && strobe) = (st, (emptyWishboneS2M, st))
          -- already done, ACK and same state
          | st /= Running = (st, (emptyWishboneS2M { acknowledge = True}, st))
          -- read, this is write-only, so error, same state
          | not writeEnable =
              ( st
              , ((emptyWishboneS2M @(Bytes 4))
                  { err = True
                  , readData = errorX "status register is write-only"
                  }
                , st))
          -- write! change state, ACK
          | otherwise =
              let state = case writeData of
                    1 -> Success
                    _ -> Fail
              in (state, (emptyWishboneS2M { acknowledge = True }, state))

    {-
    0b1000xxxx 0x8x instruction memory
    0b0100xxxx 0x4x data memory
    0b1100xxxx 0xCx status register
    -}
    peConfig = PeConfig (0b10 :> 0b01 :> 0b11 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)

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

        let elfPath = root </> "firmware/examples/target/riscv32imc-unknown-none-elf/release/processing-element-test"

        memBlobsFromElf BigEndian elfPath Nothing)


vexRiscvTest ::
  "CLK_125MHZ" ::: DiffClock Basic125 ->
  "" :::
    ( "done"    ::: Signal Basic200 Bool
    , "success" ::: Signal Basic200 Bool
    )
vexRiscvTest diffClk = (testDone, testSuccess)
  where
    (clk, clkStable0) = clockWizardDifferential (SSymbol @"pll") diffClk noReset
    clkStable1 = xpmCdcSingle clk clk clkStable0 -- improvised reset syncer

    clkStableRst = unsafeFromActiveLow clkStable1

    (unbundle -> (unbundle -> (testDone, testSuccess), iBus)) =
      hwSeqX probe $
      hwSeqX dbgProbe $
      withClockResetEnable clk reset enableGen (vexRiscvInner @Basic200)

    reset = orReset clkStableRst testReset
    ((unsafeFromActiveLow -> testReset) :> Nil) = unbundle probe

    probe :: Signal Basic200 (Vec 1 Bool)
    probe = vioProbe
      (   "probe_test_done"
       :> "probe_test_success"
       :> Nil)
      ( "probe_test_start" :> Nil)
      (False :> Nil)
      clk
      testDone
      testSuccess

    dbgProbe :: Signal Basic200 (Vec 0 Bool)
    dbgProbe = vioProbe
      (   "dbg_reset"
       :> "dbg_clkStableRst"
       :> "dbg_testReset"
       :> "dbg_ibus_active"
       :> "dbg_ibus_addr"
       :> "dbg_ibus_addr_trunc"
       :> Nil)
      Nil
      Nil
      clk
      (unsafeToActiveHigh reset)
      (unsafeToActiveHigh clkStableRst)
      (unsafeToActiveHigh testReset)
      ((busCycle <$> iBus) .&&. (strobe <$> iBus))
      (addr <$> iBus)
      ((truncateB  :: BitVector 32 -> BitVector 31) . addr <$> iBus)

{-# NOINLINE vexRiscvTest #-}
makeTopEntity 'vexRiscvTest
