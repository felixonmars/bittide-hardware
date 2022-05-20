-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0


module Bittide.DoubleBufferedRAM where

import Clash.Prelude

import Bittide.SharedTypes
-- | The double buffered RAM component is a memory component that internally uses a single
-- blockRam, but enables the user to write to one part of the ram and read from another.
-- When the metacycle indicate (the first argument) is True, the read buffer and write buffer
-- are swapped. This signal should be True for the first cycle of every metacycle.
doubleBufferedRAM ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, NFDataX a) =>
  -- | The initial contents of the first buffer. The second buffer is undefined.
  Vec memDepth a ->
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Index memDepth, a)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRAM initialContent switch readAddr writeFrame = output
  where
    outputSelect = register False readSelect
    readSelect = mux switch (not <$> outputSelect) outputSelect
    writeSelect = not <$> readSelect

    writeEntries bufSelect frame
      | bufSelect = (Nothing, frame)
      | otherwise = (frame, Nothing)
    (newEntry0, newEntry1) = unbundle (writeEntries <$> writeSelect <*> writeFrame)
    buffer0 = blockRam initialContent readAddr newEntry0
    buffer1 = blockRam initialContent readAddr newEntry1

    output = mux outputSelect buffer1 buffer0

-- | The byte addressable double buffered RAM component is a memory component that has a memory width which is a multiple of 8 bits.
-- It contains a blockRam per byte and uses the one hot byte select signal to determine which bytes will be written to the blockRam.
-- This component is double buffered such that it returns the read data from both buffers in a tuple where the first element
-- contains the read data from the "active" buffer, and the second element contains the read data from the "inactive" buffer.
-- Writing to this component will always write to the inactive buffer.
doubleBufferedRAMByteAddressable ::
  forall dom depth a .
  ( KnownNat depth, HiddenClockResetEnable dom, Paddable a, ShowX a) =>
  -- | The initial contents of the first buffer.
  Vec depth a ->
  -- | Indicates when a new metacycle has started.
  Signal dom Bool ->
  -- | Read address.
  Signal dom (Index depth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Located  depth a)) ->
  -- | One hot byte select for writing only
  Signal dom (ByteEnable (Regs a 8)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRAMByteAddressable initialContent switch readAddr writeFrame byteSelect = output
 where
    outputSelect  = register False readSelect
    readSelect    = mux switch (not <$> outputSelect) outputSelect
    writeSelect   = not <$> readSelect

    writeEntries bufSelect frame = if bufSelect then (Nothing, frame) else (frame, Nothing)
    (newEntry0, newEntry1) = unbundle (writeEntries <$> writeSelect <*> writeFrame)
    buffer0 = blockRamByteAddressable initialContent readAddr newEntry0 byteSelect
    buffer1 = blockRamByteAddressable initialContent readAddr newEntry1 byteSelect

    output = mux outputSelect buffer1 buffer0

-- | Blockram similar to 'blockRam' with the addition that it takes a byte select signal
-- that controls which bytes at the write address are updated.
blockRamByteAddressable ::
  forall dom depth a .
  (HiddenClockResetEnable dom, KnownNat depth, Paddable a, ShowX a) =>
  Vec depth a ->
  Signal dom (Index depth) ->
  Signal dom (Maybe (Located depth a)) ->
  Signal dom (ByteEnable (Regs a 8)) ->
  Signal dom a
blockRamByteAddressable initRAM readAddr newEntry byteSelect =
   -- (\x y z-> trace (showX (x, y)) z) <$> readAddr <*> bundle writeBytes <*>
    registersToData @_ @8 . RegisterBank <$> readBytes
 where
   initBytes = transpose $ getBytes <$> initRAM
   getBytes (getRegs -> RegisterBank vec) = vec
   writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
   readBytes = bundle $ (`blockRam` readAddr) <$> initBytes <*> writeBytes

-- | Register similar to 'register' with the addition that it takes a byte select signal
-- that controls which bytes are updated.
registerByteAddressable ::
  forall dom a .
  (HiddenClockResetEnable dom, Paddable a) =>
  a ->
  Signal dom a ->
  Signal dom (ByteEnable (Regs a 8)) ->
  Signal dom a
registerByteAddressable initVal newVal byteEnables =
  registersToData @_ @8 . RegisterBank <$> bundle regsOut
 where
  initBytes = getBytes initVal
  newBytes = unbundle $ getBytes <$> newVal
  regsOut = (`andEnable` register) <$> unbundle (unpack <$> byteEnables) <*> initBytes <*> newBytes
  getBytes (getRegs -> RegisterBank vec) = vec

-- | Takes singular write operation (Maybe (Index maxIndex, writeData)) and splits it up
-- according to a supplied byteselect bitvector into a vector of byte sized write operations
-- (Maybe (Index maxIndex, Byte)).
splitWriteInBytes ::
  forall maxIndex writeData .
  (Paddable writeData) =>
  Maybe (Located maxIndex writeData) ->
  ByteEnable (Regs writeData 8) ->
  Vec (Regs writeData 8) (Maybe (LocatedByte maxIndex))
splitWriteInBytes (Just (addr, writeData)) byteSelect =
  case paddedToRegisters $ Padded writeData of
    RegisterBank vec -> splitWrites <$> unpack byteSelect <*> vec
     where
      splitWrites :: Bool -> Byte -> Maybe (LocatedByte maxIndex)
      splitWrites b bv = if b then Just (addr, bv) else Nothing

splitWriteInBytes Nothing _ = repeat Nothing
