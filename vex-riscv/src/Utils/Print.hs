-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Println-debugging during simulation
module Utils.Print (performPrintsToStdout, getPrintContents) where

import Clash.Prelude
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Word (Word8)

-- | Read all printed data to an @address@ into a 'ByteString'
getPrintContents ::
  -- | How many bytes to sample
  Int ->
  -- | Address to use as the character-device address
  Unsigned 32 ->
  -- | All write operations (addr, data)
  [Maybe (Unsigned 32, Signed 32)] ->
  BS.ByteString
getPrintContents n addr =
  BS.pack
    . L.take n
    . fmap (addrByte . snd)
    . filter (\(addr', _) -> addr' == addr)
    . catMaybes

-- | Prints all data written to an @address@ to @stdout@.
performPrintsToStdout ::
  -- | Address to use as the character-device address
  Unsigned 32 ->
  -- | All write operations (addr, data)
  [Maybe (Unsigned 32, Signed 32)] ->
  IO ()
performPrintsToStdout addr =
  traverse_ (pChar . snd)
    . filter (\(addr', _) -> addr' == addr)
    . catMaybes

pChar :: Signed 32 -> IO ()
pChar = BS.putStr . BS.singleton . addrByte

-- take one byte
addrByte :: Signed 32 -> Word8
addrByte = bitCoerce . slice d7 d0
