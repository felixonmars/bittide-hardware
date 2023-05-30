  -- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Bittide.Instances.ProcessingElement where

import Clash.Prelude

import Data.Bifunctor
import Clash.Annotations.TH
import Language.Haskell.TH
import Paths_bittide_instances
import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Wishbone
import Bittide.SharedTypes
import Bittide.Axi4
import Protocols
import Protocols.Internal
import Protocols.Axi4.Stream
import Clash.Xilinx.ClockGen
import Clash.Cores.Xilinx.Extra

vexRiscUartEcho ::
  "SYSCLK_300_N" ::: Clock Basic300 ->
  "SYSCLK_300_P" ::: Clock Basic300 ->
  "CPU_RESET" ::: Reset Basic300 ->
  ("USB_UART_TX" ::: CSignal Basic200 Bit, CSignal Basic200 ()) ->
  (CSignal Basic200 (), "USB_UART_RX" ::: CSignal Basic200 Bit)
vexRiscUartEcho clk_n clk_p rst_in =
  toSignals $ withClockResetEnable clk200 rst200 enableGen $
    circuit $ \uartRx -> do
      [uartBus] <- (processingElement @Basic200 peConfig) -< ()
      (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
      idC -< uartTx
 where
  clk300 = ibufds clk_p clk_n
  rst300 = resetGlitchFilter d1024 clk300 rst_in
  (clk200, pllLock) = clockWizard (SSymbol @"pll_300_200") clk300 rst300
  rst200 = resetSynchronizer clk200 (unsafeFromLowPolarity pllLock)

  (  (_iStart, _iSize, iMem)
   , (_dStart, _dSize, dMem)) = $(do
      elfPath <- runIO $ getDataFileName "data/binaries/hello"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig = PeConfig (0 :> 1 :> 2 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)

makeTopEntity 'vexRiscUartEcho



vexRiscAxiLoopback::
  "SYSCLK_300_N" ::: Clock Basic300 ->
  "SYSCLK_300_P" ::: Clock Basic300 ->
  "CPU_RESET" ::: Reset Basic300 ->
  ("USB_UART_TX" ::: CSignal Basic200 Bit, CSignal Basic200 ()) ->
  (CSignal Basic200 (), "USB_UART_RX" ::: CSignal Basic200 Bit)
vexRiscAxiLoopback clk_n clk_p rst_in =
  toSignals $ withClockResetEnable clk200 rst200 enableGen $
    circuit $ \uartRx -> do
      [uartBus, axiTxBus, axiRxBus] <- (processingElement @Basic200 peConfig) -< ()
      axi <- Circuit addTUserFalse <| axisFromByteStream <| axisToByteStream <| wbToAxiTx -< axiTxBus
      _axiRxStatus <- wbAxisRxBufferCircuit d64 -< (axiRxBus, axi)
      (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @9600) -< (uartBus, uartRx)
      idC -< uartTx
 where
  clk300 = ibufds clk_p clk_n
  rst300 = resetGlitchFilter d64 clk300 rst_in
  (clk200, pllLock) = clockWizard (SSymbol @"pll_300_200") clk300 rst300
  rst200 = resetSynchronizer clk200 (unsafeFromLowPolarity pllLock)

  (  (_iStart, _iSize, iMem)
   , (_dStart, _dSize, dMem)) = $(do
      elfPath <- runIO $ getDataFileName "data/binaries/axi-loopback"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig = PeConfig indicesI (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)
  addTUserFalse (m2sLeft, s2mRight) = (s2mRight, fmap (fmap (\a -> a{_tuser = False})) m2sLeft)

makeTopEntity 'vexRiscAxiLoopback

vexRiscTcpLoopback::
  "SYSCLK_300_N" ::: Clock Basic300 ->
  "SYSCLK_300_P" ::: Clock Basic300 ->
  "CPU_RESET" ::: Reset Basic300 ->
  ("USB_UART_TX" ::: CSignal Basic25 Bit, CSignal Basic25 ()) ->
  (CSignal Basic25 (), "USB_UART_RX" ::: CSignal Basic25 Bit)
vexRiscTcpLoopback clk_n clk_p rst_in =
  toSignals $ withClockResetEnable clk200 rst200 enableGen $
    circuit $ \uartRx -> do
      [uartBus, axiTxBus, axiRxBus, timeBus] <- (processingElement @Basic25 peConfig) -< ()
      axi <- Circuit addTUserFalse <| axisFromByteStream <| axiFifo (SNat @1024) <| axisToByteStream <| wbToAxiTx -< axiTxBus
      _axiRxStatus <- wbAxisRxBufferCircuit (SNat @256) -< (axiRxBus, axi)
      timeWb -< timeBus
      (uartTx, _uartStatus) <- uartWb d128 d128 (SNat @921600) -< (uartBus, uartRx)
      idC -< uartTx
 where
  clk300 = ibufds clk_p clk_n :: Clock Basic300
  rst300 = resetGlitchFilter d64 clk300 rst_in
  (clk200, pllLock) = clockWizard (SSymbol @"pll_300_200") clk300 rst300
  rst200 = resetSynchronizer clk200 (unsafeFromLowPolarity pllLock)

  (  (_iStart, _iSize, iMem)
   , (_dStart, _dSize, dMem)) = $(do
      elfPath <- pure "/home/lucas/bittide-hardware/firmware/examples/target/riscv32imc-unknown-none-elf/release/smoltcp-example"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig = PeConfig indicesI (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)
  addTUserFalse (m2sLeft, s2mRight) = (s2mRight, fmap (fmap (\a -> a{_tuser = False})) m2sLeft)

makeTopEntity 'vexRiscTcpLoopback


vexRiscExposedAxi::
  "clk" ::: Clock Basic125 ->
  "rst" ::: Reset Basic125 ->
  ( "" ::: ("rx_axis" ::: Signal Basic125 ("tvalid" ::: Bool, "" ::: Axi4StreamM2S ('Axi4StreamConfig 1 0 0) Bool),"USB_UART_TX" ::: CSignal Basic125 Bit), "tx_axis_tready" ::: Signal Basic125 Axi4StreamS2M)
     -> "" ::: ("rx_axis_tready" ::: Signal Basic125 Axi4StreamS2M, "" ::: ("tx_axis" ::: Signal Basic125 ("tvalid" ::: Bool, "" ::: Axi4StreamM2S ('Axi4StreamConfig 1 0 0) ()), "USB_UART_RX" ::: CSignal Basic125 Bit))
vexRiscExposedAxi clk rst = case (withClockResetEnable clk rst enableGen $
 circuit $ \(axiRx1, uartRx) -> do
  [uartBus, axiTxBus, axiRxBus, timeBus] <- (processingElement @Basic125 peConfig) -< ()
  axiTx1 <- axisToByteStream <| wbToAxiTx -< axiTxBus
  axiRx4 <- axisFromByteStream @_ @4 @0 @0 -< axiRx1
  _axiRxStatus <- wbAxisRxBufferCircuit (SNat @400) -< (axiRxBus, axiRx4)
  timeWb -< timeBus
  (uartTx, _uartStatus) <- uartWb d128 d128 (SNat @921600) -< (uartBus, uartRx)
  idC -< (axiTx1, uartTx)) of
    Circuit go -> uncurry $ \fwdLeft bwdLeft -> case (go (first (fmap bitCoerce) fwdLeft, (bwdLeft, CSignal $ pure ()))) of
      ((bwdRight, _), fwdRight) -> (bwdRight, first (fmap bitCoerce) fwdRight)
 where
  (  (_iStart, _iSize, iMem)
   , (_dStart, _dSize, dMem)) = $(do
      elfPath <- pure "/home/lucas/bittide-hardware/firmware/examples/target/riscv32imc-unknown-none-elf/release/smoltcp-example"
      memBlobsFromElf BigEndian elfPath Nothing)

  peConfig = PeConfig indicesI (Reloadable $ Blob iMem) (Reloadable $ Blob dMem)


makeTopEntity 'vexRiscExposedAxi
