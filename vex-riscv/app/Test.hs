-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

import Clash.Prelude hiding (not, (&&))
import Prelude as P hiding ((||))

import Clash.Explicit.BlockRam.File (memFile)
import Clash.Signal.Internal (Signal((:-)))

import Bittide.DoubleBufferedRam (InitialContent(..), ContentType(..), wbStorage')
import Bittide.SharedTypes hiding (delayControls)
import Bittide.Wishbone (singleMasterInterconnect')

import Utils.Print (performPrintsToStdout)
import Utils.ReadElf
import VexRiscv

import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified GHC.TypeNats as TN

import GHC.Base (assert)
import GHC.IO.Handle
import GHC.Stack (HasCallStack)
import Protocols.Wishbone
import System.Directory (removeFile)
import System.Environment
import System.IO (openTempFile)
import Control.Monad (forM_)
import Control.Monad (when)
import Text.Printf (printf)

emptyInput :: Input
emptyInput =
  Input
    { timerInterrupt = low,
      externalInterrupt = low,
      softwareInterrupt = low,
      iBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0},
      dBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0}
    }

emptyOutput :: Output
emptyOutput =
  Output {iBusWbM2S=emptyWishboneM2S, dBusWbM2S=emptyWishboneM2S}

type Memory dom = (BitVector 32, Signal dom (WishboneM2S 32 4 (BitVector 32))
  -> Signal dom (WishboneS2M (BitVector 32)))

cpu
  :: (HasCallStack, HiddenClockResetEnable dom)
  => Memory dom
  -> Memory dom
  -> ( Signal dom Output
     -- writes
     , Signal dom (Maybe (BitVector 32, BitVector 32))
     -- iBus responses
     , Signal dom (WishboneS2M (BitVector 32))
     -- dBus responses
     , Signal dom (WishboneS2M (BitVector 32))
     )
cpu (iMemStart, iMem) (dMemStart, dMem) = (output, writes, iWb, dWb)
  where
    output = vexRiscv input
    dM2S = dBusWbM2S <$> output
    dWb = dS2M
    iWb = iMem (mapAddr (\x -> x - iMemStart) . unBusAddr . iBusWbM2S <$> output)

    (dummyAddr, dummy) = dummyWb 0x70000000
    dummyS2M = dummy dummyM2S
    dMemS2M = dMem dMemM2S

    dSlaveS2Ms = bundle (dummyS2M :> dMemS2M :> Nil)

    (dS2M, unbundle -> (dummyM2S :> dMemM2S :> Nil)) = singleMasterInterconnect' (dummyAddr :> dMemStart :> Nil) (unBusAddr <$> dM2S) dSlaveS2Ms

    input =
      ( \iBus dBus ->
          Input
            { timerInterrupt = low,
              externalInterrupt = low,
              softwareInterrupt = low,
              iBusWbS2M = iBus,
              dBusWbS2M = dBus
            }
      )
        <$> iWb
        <*> dWb

    unBusAddr = mapAddr (`shiftL` 2)

    writes = mux (busCycle <$> dM2S .&&. strobe <$> dM2S .&&. writeEnable <$> dM2S .&&. acknowledge <$> dWb)
      (do
        dM2S' <- dM2S
        pure $ Just (addr dM2S' `shiftL` 2, writeData dM2S')
        )
      (pure Nothing)

mapAddr :: (BitVector aw -> BitVector aw) -> WishboneM2S aw selWidth a -> WishboneM2S aw selWidth a
mapAddr f wb = wb { addr = f (addr wb) }

-- | Wishbone circuit that always acknowledes every request
--
-- Used for the character device. The character device address gets mapped to this
-- component because if it were to be routed to the data memory (where this address is
-- not in the valid space) it would return ERR and would halt execution.
dummyWb :: (HiddenClockResetEnable dom) => BitVector 32 -> Memory dom
dummyWb address = (address, \m2s -> delayControls m2s (reply <$> m2s))
  where
    reply WishboneM2S{..} = (emptyWishboneS2M @(BitVector 32)) { acknowledge = acknowledge, readData = 0 }
      where
        acknowledge = busCycle && strobe

    -- | Delays the output controls to align them with the actual read / write timing.
    delayControls ::
      (HiddenClockResetEnable dom, NFDataX a) =>
      Signal dom (WishboneM2S addressWidth selWidth a) -> -- current M2S signal
      Signal dom (WishboneS2M a) ->
      Signal dom (WishboneS2M a)
    delayControls m2s s2m0 = mux inCycle s2m1 (pure emptyWishboneS2M)
     where
      inCycle = (busCycle <$> m2s) .&&. (strobe <$> m2s)
      delayedAck = register False (acknowledge <$> s2m0 .&&. (not <$> delayedAck))
      delayedErr = register False (err <$> s2m0)
      s2m1 = (\wb newAck newErr-> wb{acknowledge = newAck, err = newErr})
        <$> s2m0 <*> delayedAck <*> delayedErr

loadProgram :: (HiddenClockResetEnable dom) => FilePath -> IO (IO (), Memory dom, Memory dom)
loadProgram path = do
  elfBytes <- BS.readFile path
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  assert (entry == 0x80000000) (pure ())

  (iPath, iHandle) <- openTempFile "/tmp" "imem.blob"

  (d0Path, d0Handle) <- openTempFile "/tmp" "dmem.0.blob"
  (d1Path, d1Handle) <- openTempFile "/tmp" "dmem.1.blob"
  (d2Path, d2Handle) <- openTempFile "/tmp" "dmem.2.blob"
  (d3Path, d3Handle) <- openTempFile "/tmp" "dmem.3.blob"

  let removeFiles = do
        removeFile iPath
        removeFile d0Path
        removeFile d1Path
        removeFile d2Path
        removeFile d3Path

  let
    -- endian swap instructions
    iMemBS = memFile Nothing $ L.map (\[a, b, c, d] -> bitCoerce (d, c, b, a) :: BitVector 32) $ chunkFill 4 0 (content iMem)

    (dL0, dL1, dL2, dL3) = split4 $ content dMem
    dMem0BS = memFile Nothing dL0
    dMem1BS = memFile Nothing dL1
    dMem2BS = memFile Nothing dL2
    dMem3BS = memFile Nothing dL3

    iMemStart = startAddr iMem
    dMemStart = startAddr dMem

    iMemSize = I.size iMem `divRU` 4
    dMemSize = I.size dMem `divRU` 4

    dContentVec = d0Path :> d1Path :> d2Path :> d3Path :> Nil

  -- write data to files
  hPutStr iHandle iMemBS
  -- endian swap data
  hPutStr d0Handle dMem3BS
  hPutStr d1Handle dMem2BS
  hPutStr d2Handle dMem1BS
  hPutStr d3Handle dMem0BS

  -- close files
  hClose iHandle
  hClose d0Handle
  hClose d1Handle
  hClose d2Handle
  hClose d3Handle

  let
    instrMem = case TN.someNatVal (toEnum iMemSize) of
      SomeNat (snatProxy -> depth) ->
        case compareSNat depth d1 of
          SNatLE -> error "should not happen"
          SNatGT ->
            let
              initContent = helper depth $ Reloadable $ File iPath
            in (iMemStart, wbStorage' initContent)

    dataMem = case TN.someNatVal (toEnum dMemSize) of
      SomeNat (snatProxy -> depth) ->
        case compareSNat depth d1 of
          SNatLE -> error "should not happen"
          SNatGT ->
            let initContent = helper depth $ NonReloadable $ FileVec dContentVec
            in (dMemStart, wbStorage' initContent)


  pure (removeFiles, instrMem, dataMem)
 where

  helper :: SNat depth -> InitialContent depth (BitVector 32) -> InitialContent depth (BitVector 32)
  helper SNat cont = cont

  startAddr :: BinaryData -> BitVector 32
  startAddr bin = resize . bitCoerce $ fst . L.head $ I.toAscList bin

  content :: BinaryData -> [BitVector 8]
  content bin = L.map snd $ I.toAscList bin

  split4 :: [BitVector 8] -> ([BitVector 8], [BitVector 8], [BitVector 8], [BitVector 8])
  split4 xs = L.unzip4 $ L.map (\[a, b, c, d] -> (a, b, c, d)) $ chunkFill 4 0 xs

  chunkFill :: Int -> a -> [a] -> [[a]]
  chunkFill _ _    [] = []
  chunkFill n fill xs =
    let (first0, rest) = L.splitAt n xs
        first1 = first0 <> L.replicate (n - L.length first0) fill
    in first1 : chunkFill n fill rest


main :: IO ()
main = do
  elfFile <- L.head <$> getArgs

  (removeFiles, iMem, dMem) <- withClockResetEnable @System clockGen resetGen enableGen $ loadProgram @System elfFile

  -- add device tree as a memory mapped component

  -- TODO read the device tree file from command line args?
  -- deviceTreePath <- getDataFileName "devicetree/contranomy-sim.dts"

  -- compileRes <- compileDeviceTreeSource deviceTreePath
  -- deviceTreeRaw <- maybe exitFailure pure compileRes

  -- deviceTreeRaw <- BS.readFile "../devicetree/blobs/contranomy-sim.dtb"
  -- add padding to prevent uninitialised accesses
  -- let padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0
  --     deviceTree = fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding

  -- let dMem' = dMem -- `I.union` I.fromAscList (L.zip [fdtAddr ..] deviceTree)

  let all@(unbundle -> (_circuit, writes, _iBus, _dBus)) = withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
        bundle (cpu iMem dMem)

  -- {-
  let init = 130
  let skipUninteresting = 0 -- 700
  let takeInteresting = 100

  let total = init + skipUninteresting + takeInteresting

  let skipTotal = init + skipUninteresting

  forM_ (L.zip [0..] $ L.take total $ sample_lazy @System all) $ \(i, (out, _, iBusS2M, dBusS2M)) -> do

    let doPrint = i >= skipTotal

    -- {-
    when doPrint $ do
      let iBus = iBusWbM2S out
      let iAddr = toInteger (addr iBus) -- `shiftL` 2
      let iValid = busCycle iBus && strobe iBus
      let iResp =
            if
            | acknowledge iBusS2M -> "ACK  "
            | err iBusS2M -> "ERR  "
            | retry iBusS2M -> "RETRY"
            | otherwise -> "NONE "

      let iRespData =
            if acknowledge iBusS2M then
              printf "% 8X" (toInteger $ readData iBusS2M)
            else
              "<no data>"

      putStr $ "iM2S:   (" <> (if not iValid then "!" else " ") <> "V) "
                <> "(" <> showX (busSelect iBus) <> ") "
                <> printf "% 8X" iAddr
                <> " (" <> printf "%X" (iAddr `shiftL` 2) <> ")"
      putStrLn $ "            - iS2M: " <> iResp <> " - " <> iRespData
    -- -}

    -- {-
    when doPrint $ do
      let dBus = dBusWbM2S out
      let dAddr = toInteger (addr dBus) -- `shiftL` 2
      let dWrite = writeEnable dBus
      let dValid = busCycle dBus && strobe dBus

      let mode = if dWrite then "W" else "R"

      let dResp =
            if
            | acknowledge dBusS2M -> "ACK  "
            | err dBusS2M -> "ERR  "
            | retry dBusS2M -> "RETRY"
            | otherwise -> "NONE "

      let dRespData =
            if acknowledge dBusS2M then
              printf "% 8X" (toInteger $ readData dBusS2M)
            else
              "<no data>"

      let writeDat =
            if dValid && dWrite then
              printf "% 8X" (toInteger $ writeData dBus)
            else
              " no data"

      when (dValid || hasTerminateFlag dBusS2M) $ do
        putStr $ "dM2S: " <> mode <> " (" <> (if not dValid then "!" else " ") <> "V) "
                  <> "(" <> showX (busSelect dBus) <> ") "
                  <> printf "%X" dAddr
                  <> " (" <> printf "%X" (dAddr `shiftL` 2) <> ") "
                  <> "<" <> writeDat <> "> - "
        putStrLn $ "dS2M: " <> dResp <> " - " <> dRespData
    -- -}

  -- -}

  -- performPrintsToStdout 0x70000000 (sample_lazy $ bitCoerce <$> writes)

  removeFiles
