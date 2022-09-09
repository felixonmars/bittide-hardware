
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ApplicativeDo #-}

import Clash.Prelude hiding ((||), empty, not, (&&))
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import qualified Data.List as L
import GHC.Stack (HasCallStack)
import qualified GHC.TypeNats as TN

import Protocols.Wishbone
import Protocols.Wishbone.Standard (memoryWb)
import System.Environment
import Utils.ReadElf
import VexRiscv
import Prelude as P
import Bittide.Extra.Wishbone (wishboneStorage', wishboneStorage, DWord)
import Clash.Signal.Internal (Signal((:-)))
import Text.Printf (printf)
import Data.IntMap.Strict (toAscList)
import GHC.Natural
import Bittide.DoubleBufferedRam (InitialContent(..), wbStorage')
import Clash.Sized.Vector (unsafeFromList)
import Debug.Trace (trace)
import Protocols.Wishbone.Standard.Hedgehog (driveStandard, WishboneMasterRequest (Read, Write), stallStandard)
import Protocols.Hedgehog (defExpectOptions, ExpectOptions (eoResetCycles))
import Protocols (Circuit(Circuit), (|>))

emptyInput :: Input
emptyInput =
  Input
    { timerInterrupt = low,
      externalInterrupt = low,
      softwareInterrupt = low,
      iBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0},
      dBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0}
    }

definedS2M :: forall n. (KnownNat n) => WishboneS2M (BitVector n)
definedS2M = (emptyWishboneS2M @(BitVector n)) { readData = 0 :: BitVector n }

definedM2S :: (KnownNat addrWidth, KnownNat n) =>
  WishboneM2S addrWidth (BitSize (BitVector n) `DivRU` 8) (BitVector n)
definedM2S =   WishboneM2S
    { addr = 0,
      writeData = 0,
      busSelect = maxBound,
      lock = False,
      busCycle = False,
      strobe = False,
      writeEnable = False,
      cycleTypeIdentifier = Classic,
      burstTypeExtension = LinearBurst
    }

withDWordVec :: BinaryData -> (forall size. (KnownNat size) => BitVector 32 -> Vec size (BitVector 32) -> a) -> a
withDWordVec content fn = case TN.someNatVal size of
    SomeNat (snatProxy -> depth) ->
      fn start (helper depth (unsafeFromList words))
  where
    (start, size, words) = contents

    helper :: SNat s -> Vec s a -> Vec s a
    helper SNat v = v

    contents :: (BitVector 32, Natural, [BitVector 32])
    contents = (resize $ bitCoerce startAddr, toEnum (I.size content) `div` 4, datB4 dat)

    startAddr = fst . L.head $ ascList
    ascList = toAscList content

    dat = L.map snd ascList

    datB4 :: [BitVector 8] -> [BitVector 32]
    datB4 [] = []
    datB4 [a] = [bitCoerce (a, 0 :: BitVector 24)]
    datB4 [a, b] = [bitCoerce (a, b, 0 :: BitVector 16)]
    datB4 [a, b, c] = [bitCoerce (a, b, c, 0 :: BitVector 8)]
    datB4 (a:b:c:d:rest) = bitCoerce (a, b, c, d) : datB4 rest

mkStorageStall
  :: forall dom. (HiddenClockResetEnable dom)
  => BinaryData
  -> Bool
  -> Signal dom (WishboneM2S 32 4 (BitVector 32))
  -> Signal dom (WishboneS2M (BitVector 32))
mkStorageStall content reloadable m2s = fst (fn (m2s, ()))
  where
    storageC :: Circuit (Wishbone dom 'Standard 32 (BitVector 32)) ()
    storageC = Circuit $ \(m2s, ()) -> (mkStorage content reloadable m2s, ())
    Circuit fn = stallStandard (L.repeat 2) |> storageC

mkStorage
  :: (HiddenClockResetEnable dom)
  => BinaryData
  -> Bool
  -> Signal dom (WishboneM2S 32 4 (BitVector 32))
  -> Signal dom (WishboneS2M (BitVector 32))
mkStorage content reloadable m2s =

  trace ("Start " <> show start) $
  case TN.someNatVal size of
    SomeNat (snatProxy -> depth) ->
      case compareSNat depth (SNat @1) of
        SNatLE -> error "aaaaa"
        SNatGT ->
          let vec = unsafeFromList words
              initCont = helper depth (if reloadable then Reloadable vec else NonReloadable vec)
          in
          endianSwap' <$> wbStorage' depth initCont (mapAddr addrFn <$> m2s)
  where
    endianSwap' :: WishboneS2M (BitVector 32) -> WishboneS2M (BitVector 32)
    endianSwap' s2m = s2m { readData = endianSwap $ readData s2m }

    endianSwap :: BitVector 32 -> BitVector 32
    endianSwap (bitCoerce -> (a, b, c, d)) =
      bitCoerce (d :: BitVector 8, c :: BitVector 8, b :: BitVector 8, a :: BitVector 8)

    addrFn :: BitVector 32 -> BitVector 32
    addrFn addr = addr - start -- (start `shiftR` 2)

    helper :: SNat depth -> InitialContent depth (BitVector 32) -> InitialContent depth (BitVector 32)
    helper SNat cont = cont

    (start, size, words) = contents

    contents :: (BitVector 32, Natural, [BitVector 32])
    contents = (resize $ bitCoerce startAddr, toEnum (I.size content) `div` 4, dat4 `deepseqX` dat4)

    startAddr = fst . L.head $ ascList
    ascList = toAscList content

    dat = L.map snd ascList
    dat4 = datB4 dat

    datB4 :: [BitVector 8] -> [BitVector 32]
    datB4 [] = []
    datB4 [a] = [bitCoerce (a, 0 :: BitVector 24)]
    datB4 [a, b] = [bitCoerce (a, b, 0 :: BitVector 16)]
    datB4 [a, b, c] = [bitCoerce (a, b, c, 0 :: BitVector 8)]
    datB4 (a:b:c:d:rest) = bitCoerce (a, b, c, d) : datB4 rest

makeDefined :: WishboneS2M (BitVector 32) -> WishboneS2M (BitVector 32)
makeDefined wb = wb { readData = readData' }
  where
    readData' =
      if hasUndefined (readData wb) then
        0
      else
        readData wb

cpu
  :: (HasCallStack, HiddenClockResetEnable dom)
  => BinaryData
  -> BinaryData
  -> ( Signal dom Output
     -- writes
     , Signal dom (Maybe (BitVector 32, BitVector 32))
     -- iBus responses
     , Signal dom (WishboneS2M (BitVector 32))
     -- dBus responses
     , Signal dom (WishboneS2M (BitVector 32))
     )
cpu iMem dMem = (output, writes, iWb, dWb)
  where
    wishboneStorageStalled name content input = makeDefined <$> fst (outFn (input, ()))
      where
        inC  = wishboneStorage name content
        Circuit outFn = stallStandard (L.repeat 1) |> inC

    wishboneStorageDelayed name content input = delayControls input wbOut
     where
      wbOut = wishboneStorage' name (content, False) input

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

    output = vexRiscv $ (emptyInput :- input)
    output' = -- register (Output {iBusWbM2S=definedM2S, dBusWbM2S=definedM2S})
      output
    dM2S = dBusWbM2S <$> output'
    dWb =
      -- wishboneStorage' "data storage" (dMem, False) (unBusAddr <$> dM2S)
      wishboneStorageDelayed "data storage" dMem (unBusAddr <$> dM2S)
      -- mkStorage dMem False (unBusAddr <$> dM2S)
    iWb =
      -- wishboneStorage' "instruction storage" (iMem, False) (unBusAddr . iBusWbM2S <$> output')
      -- wishboneStorageStalled "instruction storage" iMem (unBusAddr . iBusWbM2S <$> output')
      -- wishboneStorageDelayed "instruction storage" iMem (unBusAddr . iBusWbM2S <$> output')
      mkStorage iMem True (unBusAddr . iBusWbM2S <$> output')
      -- memoryWb' iMem (iBusWbM2S <$> output')
    input =
      ( \iWb dWb ->
          Input
            { timerInterrupt = low,
              externalInterrupt = low,
              softwareInterrupt = low,
              iBusWbS2M = makeDefined iWb,
              dBusWbS2M = makeDefined dWb
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

dataRanges :: BinaryData -> [(Int, Int)]
dataRanges dat = L.reverse $ go [] (toAscList dat)
  where
    go acc [] = acc
    go [] ((key, _):rest) = go [(key, key)] rest
    go ((start, last):acc) ((key, _):rest)
      | key == last + 1 = go ((start, key):acc) rest
      | otherwise       = go ((key, key):(start, last):acc) rest

main :: IO ()
main = mainTest

mainTest :: IO ()
mainTest = do
  elfFile <- L.head <$> getArgs
  elfBytes <- BS.readFile elfFile
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  let dStart = entry + fromInteger (toInteger (I.size iMem))
  putStrLn $ "size " <> show (I.size dMem)

  let driverC = driveStandard @System @(BitVector 32) @32
                  (defExpectOptions { eoResetCycles = 2 })
                  $ cycle [Read (dStart + 0) maxBound, Read (dStart + 1) maxBound, Write (dStart + 0) maxBound 1, Write (dStart + 1) maxBound 2]
      Circuit driverFn = driverC
      driver s2m = snd $ driverFn ((), s2m)

      dMemSt = withClockResetEnable clockGen resetGen enableGen mkStorage dMem False (driver dMemSt)
      fullCircuit = liftA2 (,) (driver dMemSt) dMemSt


  forM_ (sampleN_lazy @System 10 fullCircuit) $ \(m2s, s2m) -> do
      putStrLn $ showX m2s
      putStrLn $ showX s2m
      putStrLn ""

mainCPU :: IO ()
mainCPU = do
  elfFile <- L.head <$> getArgs
  elfBytes <- BS.readFile elfFile
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  forM_ (dataRanges iMem) $ \(start, end) -> do
    putStrLn $ printf "Inst: %X - %X" start end


  forM_ (dataRanges dMem) $ \(start, end) -> do
    putStrLn $ printf "Data: %X - %X" start end

  -- add device tree as a memory mapped component

  -- TODO read the device tree file from command line args?
  -- deviceTreePath <- getDataFileName "devicetree/contranomy-sim.dts"

  -- compileRes <- compileDeviceTreeSource deviceTreePath
  -- deviceTreeRaw <- maybe exitFailure pure compileRes

  -- deviceTreeRaw <- BS.readFile "../devicetree/blobs/contranomy-sim.dtb"
  -- add padding to prevent uninitialised accesses
  -- let padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0
  --     deviceTree = fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding

  let dMem' = dMem -- `I.union` I.fromAscList (L.zip [fdtAddr ..] deviceTree)

  let all@(unbundle -> (_circuit, writes, _iBus, _dBus)) = withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
        bundle (cpu iMem dMem')

  -- {-
  forM_ (L.take 100000 $ sample_lazy @System all) $ \(out, _, iBusS2M, dBusS2M) -> do

    --
    {-
    do
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

      putStr $ "iM2S: (" <> (if not iValid then "!" else " ") <> "V) "
                <> "(" <> showX (busSelect iBus) <> ") "
                <> printf "% 8X" iAddr
                <> " (" <> printf "%X" (iAddr `shiftL` 2) <> ") - "
      putStrLn $ "iS2M: " <> iResp <> " - " <> iRespData
    -- -}

    -- {-
    do
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

  -- hookPrint 0x70000000 (sample_lazy @System (bitCoerce <$> writes))
