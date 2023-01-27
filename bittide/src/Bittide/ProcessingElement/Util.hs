module Bittide.ProcessingElement.Util where

import Clash.Prelude hiding (Exp)
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.IntMap as I

import Bittide.ProcessingElement.DeviceTreeCompiler
import Bittide.ProcessingElement.ReadElf

import System.Exit
import Bittide.SharedTypes
import Language.Haskell.TH
import Clash.Explicit.BlockRam.File
import Paths_bittide
import Numeric (showHex)

-- | Given the name of an elf file in @../target/riscv32imc-unknown-none-elf/release/@
-- , the name of a device tree in @device-trees/@ and a starting address for the device tree.
-- Return a 3 tuple containing (initial program counter, instruction memory blob, data memory blob)
memBlobsFromElf :: FilePath -> String -> I.Key -> Q Exp
memBlobsFromElf elfName deviceTreeName fdtAddr = do
  elfPath <- runIO $ getDataFileName ("../target/riscv32imc-unknown-none-elf/release/" <> elfName)
  deviceTreePath <- runIO $ getDataFileName ("device-trees/" <> deviceTreeName <> ".dts")
  (pc, iMemIntMap, dMemIntMap) <- runIO (getBytesMems elfPath deviceTreePath fdtAddr)
  let
    iResult = intMapToMemBlob iMemIntMap
    dResult = intMapToMemBlob dMemIntMap

  [|(pc, $iResult, $dResult)|]

-- | Given the name of an elf file in @../target/riscv32imc-unknown-none-elf/release/@
-- , the name of a device tree in @device-trees/@ and a starting address for the device tree.
-- Return a 3 tuple containing (initial program counter, instruction memory blob, data memory blob)
getBytesMems :: FilePath -> String -> I.Key -> IO (BitVector 32, I.IntMap Byte, I.IntMap Byte)
getBytesMems elfPath deviceTreePath fdtAddr = do

  elfBytes <- BS.readFile elfPath
  let (entry, iMem, dMem0) = readElfFromMemory elfBytes

  -- add device tree as a memory mapped component
  deviceTree <- readDeviceTree deviceTreePath
  let
    deviceTreeMap = I.fromAscList (L.zip [fdtAddr ..] deviceTree)
    dMem1 = dMem0 `I.union` deviceTreeMap

  putStrLn $ "elf file: " <> elfPath <>
          "\ndevice tree: " <> deviceTreePath <>
          "\ndevice tree starting address: " <> show fdtAddr <>
          "\n device tree size: " <> showHex (L.length deviceTree) ""

  pure (entry, iMem, dMem1)

-- | Given the name of an elf file in @../target/riscv32imc-unknown-none-elf/release/@
-- , the name of a device tree in @device-trees/@ and a starting address for the device tree.
-- Return a 3 tuple containing (initial program counter, instruction memory blob, data memory blob)
intMapToMemBlob :: I.IntMap Byte -> Q Exp
intMapToMemBlob intMap = do
  let
    (startAddr, size, mapAsList) = extractIntMapData intMap
    memBlob = memBlobTH Nothing mapAsList
  [| (startAddr, size, $memBlob) |]

-- | Write a list of `Byte`s to a file to be used with `blockRamFile`.
writeByteListToFile :: FilePath -> [Byte] -> IO ()
writeByteListToFile filePath byteList = do
  let fileContent = memFile Nothing byteList
  writeFile filePath fileContent

-- | Given an IntMap, return the starting address, size and content as @[Bytes 4]@
extractIntMapData ::
  -- | IntMap
  I.IntMap (BitVector 8) ->
  -- |
  -- 1. Starting address
  -- 2. Size
  -- 3. List of words
  (BitVector 32, Int, [Bytes 4])
extractIntMapData dataMap = (resize . bitCoerce $ startAddr, size, toBytes4 content)
 where
  ordList = I.toAscList dataMap
  startAddr = fst $ L.head ordList
  size = I.size dataMap
  content =
    snd (L.head ordList) : flattenContent startAddr (L.tail ordList)

  flattenContent _ [] = []
  flattenContent prevAddr ((nextAddr, val):vals) =
    let
      n = nextAddr - prevAddr - 1
      padding = L.replicate n 0
    in padding L.++ (val : flattenContent nextAddr vals)

  toBytes4 :: [Bytes 1] -> [Bytes 4]
  toBytes4 [] = []
  toBytes4 [!a] = [bitCoerce (a, 0 :: Bytes 3)]
  toBytes4 [!a, !b] = [bitCoerce (a, b, 0 :: Bytes 2)]
  toBytes4 [!a, !b, !c] = [bitCoerce (a, b, c, 0 :: Bytes 1)]
  toBytes4 ((!a):(!b):(!c):(!d):rest) = bitCoerce (a, b, c, d) : toBytes4 rest

-- | Given the filepath to a device tree, return the divce tree as list of `Byte`.
readDeviceTree :: FilePath -> IO [Byte]
readDeviceTree deviceTreePath = do
    compileRes <- compileDeviceTreeSource deviceTreePath

    deviceTreeRaw <- maybe exitFailure pure compileRes

    let
      padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0

    pure $ (fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding)
