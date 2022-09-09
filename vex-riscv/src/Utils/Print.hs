-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Println-debugging during simulation
module Utils.Print (hookPrint, getDataBytes) where

import Clash.Prelude
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Word (Word8)

getDataBytes ::
  -- | How many bytes to sample
  Int ->
  -- | Address
  Unsigned 32 ->
  -- | Data
  [Maybe (Unsigned 32, Signed 32)] ->
  BS.ByteString
getDataBytes n addr =
  BS.pack
    . L.take n
    . fmap (addrByte . snd)
    . filter (\(addr', _) -> addr' == addr)
    . catMaybes

hookPrint ::
  -- | Address
  Unsigned 32 ->
  -- | Data
  [Maybe (Unsigned 32, Signed 32)] ->
  IO ()
hookPrint addr =
  traverse_ (pChar . snd)
    . filter (\(addr', _) -> addr' == addr)
    . catMaybes

pChar :: Signed 32 -> IO ()
pChar = BS.putStr . BS.singleton . addrByte

-- take one byte
addrByte :: Signed 32 -> Word8
addrByte = bitCoerce . slice d7 d0
