-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=7#-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Bittide.DoubleBufferedRam where

import Clash.Prelude

import Bittide.Extra.Wishbone
import Data.Maybe

import Bittide.SharedTypes
import Data.Bifunctor

data InitialContent n a where
  NonReloadable :: Vec n a -> InitialContent n a
  Reloadable :: Vec n a -> InitialContent n a
  Undefined :: (1 <= n, KnownNat n) => InitialContent n a

getContent :: InitialContent n a -> Vec n a
getContent (Reloadable v) = v
getContent (NonReloadable v) = v
getContent (Undefined) = error "getContent: Content is Undefined."

contentGenerator ::
  forall dom romSize targetSize a .
  ( HiddenClockResetEnable dom
  , KnownNat targetSize,  1 <= targetSize
  , KnownNat romSize, romSize <= targetSize
  , NFDataX a) =>
  Vec romSize a ->
  (Signal dom (Maybe (Located targetSize a)), Signal dom Bool)
contentGenerator Nil = (pure Nothing, pure True)
contentGenerator content@(Cons _ _) = (mux (running .&&. not <$> done) writeOp (pure Nothing), done)
 where
  running = register False $ pure True
  writeOp = curry Just . resize <$> romAddr0 <*> element
  done = register False (done .||. (running .&&. romAddr0 .==. pure maxBound))
  romAddr0 = register (maxBound :: Index romSize) romAddr1
  romAddr1 = satSucc SatWrap <$> romAddr0
  element = rom content (bitCoerce <$> romAddr1)

-- | Dual-ported Wishbone storage element, essentially a wrapper for the single-ported version
-- which priorities port B over port A. While port B is making a transaction, port A will be ignored.
wbStorageDP ::
  forall dom depth initDepth aw .
  ( HiddenClockResetEnable dom
  , KnownNat aw, 2 <= aw
  , KnownNat depth, 1 <= depth
  , KnownNat initDepth, 1 <= initDepth
  , initDepth <= depth) =>
  SNat depth ->
  InitialContent initDepth (Bytes 4) ->
  Signal dom (WishboneM2S 4 aw) ->
  Signal dom (WishboneM2S 4 aw) ->
  (Signal dom (WishboneS2M 4),Signal dom (WishboneS2M 4))
wbStorageDP d@SNat initial aM2S bM2S = (aS2M, bS2M)
 where
  storageOut = wbStorage d initial storageIn
  aActive = strobe <$> aM2S .&&. busCycle <$> aM2S
  bActive = strobe <$> bM2S .&&. busCycle <$> bM2S
  aWriting = aActive .&&. writeEnable <$> aM2S

  storageIn = mux (not <$> bActive) (noWrite <$> aM2S) bM2S

  aS2M = mux (not <$> bActive) (writeIsErr <$> storageOut <*> aWriting) (noAck <$> storageOut)
  bS2M = storageOut

  noAck wb = wb{acknowledge = False, err = False}
  noWrite wb = wb{writeEnable = False}
  writeIsErr wb write = wb{err = err wb || write}

-- | Storage element with a single wishbone port, it is half word addressable to work with
-- RISC-V's C extension.
wbStorage ::
  forall dom depth initDepth aw .
  ( HiddenClockResetEnable dom
  , KnownNat aw, 2 <= aw
  , KnownNat depth, 1 <= depth
  , KnownNat initDepth, 1 <= initDepth
  , initDepth <= depth) =>
  SNat depth ->
  InitialContent initDepth (Bytes 4) ->
  Signal dom (WishboneM2S 4 aw) ->
  Signal dom (WishboneS2M 4)
wbStorage SNat initContent wbIn = delayControls wbOut
 where
  depth = resize $ bitCoerce (maxBound :: Index depth)
  romOut = bundle $ contentGenerator (getContent initContent)

  readDataA = ramA readAddrA writeEntryA byteSelectA
  readDataB = ramB readAddrB writeEntryB byteSelectB

  addElems x = x ++ replicate (SNat @(depth - initDepth))
    (errorX "wbStorage: Uninitialized element")

  (ramA, ramB, isReloadable) = case initContent of
    NonReloadable (unzip @initDepth . bitCoerce -> (b, a)) ->
      ( blockRamByteAddressable @_ @depth $ addElems a
      , blockRamByteAddressable @_ @depth $ addElems b
      , False)
    Reloadable _ ->
      (blockRamByteAddressableU, blockRamByteAddressableU, True)
    Undefined ->
      (blockRamByteAddressableU, blockRamByteAddressableU, False)

  (readAddrA, readAddrB, writeEntryA, writeEntryB, byteSelectA, byteSelectB, wbOut) =
    unbundle $ go <$> wbIn <*> readDataB <*> readDataA <*> romOut

  go WishboneM2S{..} rdB rdA romOut0 =
    (addrA, addrB, writeEntryA0, writeEntryB0, byteSelectA1, byteSelectB1, WishboneS2M{acknowledge,readData,err})
   where

    (bitCoerce . resize -> wbAddr :: Index depth, alignment) =  split @_ @(aw - 2) addr

    -- when the second lowest bit is not set, the address is considered word aligned.
    -- We don't care about the first bit to determine the address alignment because
    -- byte aligned addresses are considered illegal.
    (not -> wordAligned, byteAligned) = bimap unpack unpack $ split alignment
    addrLegal = addr <= depth && not byteAligned

    masterActive = strobe && busCycle
    err = masterActive && not addrLegal
    acknowledge = masterActive && (not isReloadable || romDone) && addrLegal
    masterWriting = masterActive && writeEnable && not err

    (bsHigh,bsLow) = split busSelect
    (writeDataHigh, writeDataLow) = split writeData

    ((addrB, byteSelectB0, writeDataB), (addrA, byteSelectA0, writeDataA))
      | wordAligned =
        ( (wbAddr, bsHigh, writeDataHigh)
        , (wbAddr, bsLow, writeDataLow))
      | otherwise =
        ( (satSucc SatBound wbAddr, bsLow, writeDataLow)
        , (wbAddr, bsHigh, writeDataHigh))

    (romWrite, romDone) = romOut0
    (romWriteB, romWriteA) = splitWrite romWrite
    (writeEntryB0, writeEntryA0)
      | isReloadable && not romDone = (romWriteB, romWriteA)
      | masterWriting = (Just (addrB, writeDataB),Just (addrB, writeDataA))
      | otherwise = (Nothing,Nothing)
    readData
      | wordAligned = rdB ++# rdA
      | otherwise   = rdA ++# rdB

    (byteSelectB1, byteSelectA1)
      | isReloadable && not romDone = (maxBound,maxBound)
      | otherwise = (byteSelectB0, byteSelectA0)

  splitWrite ::
    KnownNat a =>
    Maybe (Located n (BitVector (2*a))) ->
    (Maybe (Located n (BitVector a)), Maybe (Located n (BitVector a)))
  splitWrite (Just (i, a)) = (Just (i,upper), Just (i, lower))
   where
    (upper,lower) = split a
  splitWrite Nothing = (Nothing, Nothing)

-- | Indicates which buffer is currently selected.
data SelectedBuffer = A | B deriving (Eq, Generic, BitPack, Show, NFDataX)

flipBuffer :: SelectedBuffer -> SelectedBuffer
flipBuffer A = B
flipBuffer B = A

-- | The double buffered Ram component is a memory component that contains two buffers
-- and enables the user to write to one buffer and read from the other. 'SelectedBuffer'
-- selects which buffer is written to, while read operations read from the other buffer.
doubleBufferedRam ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, NFDataX a) =>
  -- | The initial contents of both buffers.
  Vec memDepth a ->
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom SelectedBuffer ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Index memDepth, a)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRam initialContent0 outputSelect readAddr0 writeFrame0 =
  blockRam initialContent1 readAddr1 writeFrame1
 where
  initialContent1 = concatMap (repeat @2) initialContent0
  (readAddr1, writeFrame1) =
    unbundle $ updateAddrs <$> readAddr0 <*> writeFrame0 <*> outputSelect

-- | Version of 'doubleBufferedRam' with undefined initial contents. This component
-- contains two buffers and enables the user to write to one buffer and read from the
-- other. 'SelectedBuffer' selects which buffer is written to, while read operations
-- read from the other buffer.
doubleBufferedRamU ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, NFDataX a) =>
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom SelectedBuffer ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Index memDepth, a)) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRamU outputSelect readAddr0 writeFrame0 =
  blockRamU NoClearOnReset (SNat @(2 * memDepth)) rstFunc readAddr1 writeFrame1
 where
  (readAddr1, writeFrame1) =
    unbundle $ updateAddrs <$> readAddr0 <*> writeFrame0 <*> outputSelect
  rstFunc = const (errorX "doubleBufferedRamU: reset function undefined")

-- | The byte addressable double buffered Ram component is a memory component that
-- consists of two buffers and internally stores its elements as a multiple of 8 bits.
-- It contains a blockRam per byte and uses the one hot byte select signal to determine
-- which bytes will be overwritten during a write operation. This components writes to
-- one buffer and reads from the other. 'SelectedBuffer' selects which buffer is
-- written to, while read operations read from the other buffer.
doubleBufferedRamByteAddressable ::
  forall dom memDepth a .
  ( KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom, Paddable a, ShowX a) =>
  -- | The initial contents of the first buffer.
  Vec memDepth a ->
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom SelectedBuffer ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Located memDepth a)) ->
  -- | One hot byte select for writing only
  Signal dom (ByteEnable a) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRamByteAddressable initialContent0 outputSelect readAddr0 writeFrame0 =
  blockRamByteAddressable initialContent1 readAddr1 writeFrame1
 where
  initialContent1 = concatMap (repeat @2) initialContent0
  (readAddr1, writeFrame1) =
    unbundle $ updateAddrs <$> readAddr0 <*> writeFrame0 <*> outputSelect

-- | Version of 'doubleBufferedRamByteAddressable' where the initial content is undefined.
-- This memory element consists of two buffers and internally stores its elements as a
-- multiple of 8 bits. It contains a blockRam per byte and uses the one hot byte select
-- signal to determine which nBytes will be overwritten during a write operation.
-- This components writes to one buffer and reads from the other. Which buffer is
-- used for reading while the other is used for writing is controlled by the 'SelectedBuffer'.
doubleBufferedRamByteAddressableU ::
  forall dom memDepth a .
  ( KnownNat memDepth, 1 <= memDepth, HiddenClockResetEnable dom, Paddable a, ShowX a) =>
  -- | Controls which buffers is written to, while the other buffer is read from.
  Signal dom SelectedBuffer ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Incoming data frame.
  Signal dom (Maybe (Located  memDepth a)) ->
  -- | One hot byte select for writing only
  Signal dom (ByteEnable a) ->
  -- | Outgoing data
  Signal dom a
doubleBufferedRamByteAddressableU outputSelect readAddr0 writeFrame0 =
  blockRamByteAddressableU readAddr1 writeFrame1
 where
  (readAddr1, writeFrame1) =
    unbundle $ updateAddrs <$> readAddr0 <*> writeFrame0 <*> outputSelect

-- | Blockram similar to 'blockRam' with the addition that it takes a byte select signal
-- that controls which nBytes at the write address are updated.
blockRamByteAddressable ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, Paddable a, ShowX a) =>
  -- | Initial content.
  Vec memDepth a ->
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Write operation.
  Signal dom (Maybe (Located memDepth a)) ->
  -- | Byte enables that determine which nBytes get replaced.
  Signal dom (ByteEnable a) ->
  -- | Data at read address (1 cycle delay).
  Signal dom a
blockRamByteAddressable initRam readAddr newEntry byteSelect =
  registersToData @_ @8 . RegisterBank <$> readBytes
 where
  initBytes = transpose $ getBytes <$> initRam
  getBytes (getRegs -> RegisterBank vec) = vec
  writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
  readBytes = bundle $ (`blockRam` readAddr) <$> initBytes <*> writeBytes

-- | Version of 'blockRamByteAddressable' with undefined initial contents. It is similar
-- to 'blockRam' with the addition that it takes a byte select signal that controls
-- which nBytes at the write address are updated.
blockRamByteAddressableU ::
  forall dom memDepth a .
  (HiddenClockResetEnable dom, KnownNat memDepth, 1 <= memDepth, Paddable a, ShowX a) =>
  -- | Read address.
  Signal dom (Index memDepth) ->
  -- | Write operation.
  Signal dom (Maybe (Located memDepth a)) ->
  -- | Byte enables that determine which nBytes get replaced.
  Signal dom (ByteEnable a) ->
  -- | Data at read address (1 cycle delay).
  Signal dom a
blockRamByteAddressableU readAddr newEntry byteSelect =
  registersToData @_ @8 . RegisterBank <$> readBytes
 where
  writeBytes = unbundle $ splitWriteInBytes <$> newEntry <*> byteSelect
  readBytes = bundle $ ram readAddr <$> writeBytes
  ram = blockRamU NoClearOnReset (SNat @memDepth) rstFunc
  rstFunc = const (errorX "blockRamByteAddressableU: reset function undefined")

data RegisterWritePriority = CircuitPriority | WishbonePriority

-- | Register with additional wishbone interface, this component has a configurable
-- priority that determines which value gets stored in the register during a write conflict.
-- With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
-- collision and the wishbone bus gets acknowledged, but the value is silently ignored.
-- With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
-- fourth argument gets ignored.
registerWb ::
  forall dom a nBytes addrW .
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , 2 <= addrW) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  -- | Wishbone bus (master to slave)
  Signal dom (WishboneM2S nBytes addrW) ->
  -- | New circuit value.
  Signal dom (Maybe a) ->
  -- |
  -- 1. Outgoing stored value
  -- 2. Outgoing wishbone bus (slave to master)
  (Signal dom a, Signal dom (WishboneS2M nBytes))
registerWb writePriority initVal wbIn sigIn =
  registerWbE writePriority initVal wbIn sigIn (pure maxBound)

{-# NOINLINE registerWbE #-}
-- | Register with additional wishbone interface, this component has a configurable
-- priority that determines which value gets stored in the register during a write conflict.
-- With 'CircuitPriority', the incoming value in the fourth argument gets stored on a
-- collision and the wishbone bus gets acknowledged, but the value is silently ignored.
-- With 'WishbonePriority', the incoming wishbone write gets accepted and the value in the
-- fourth argument gets ignored. This version has an additional argument for circuit write
-- byte enables.
registerWbE ::
  forall dom a nBytes addrW .
  ( HiddenClockResetEnable dom
  , Paddable a
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , 2 <= addrW) =>
  -- | Determines the write priority on write collisions
  RegisterWritePriority ->
  -- | Initial value.
  a ->
  -- | Wishbone bus (master to slave)
  Signal dom (WishboneM2S nBytes addrW) ->
  -- | New circuit value.
  Signal dom (Maybe a) ->
  -- | Explicit Byte enables for new circuit value
  Signal dom (BitVector (Regs a 8)) ->
  -- |
  -- 1. Outgoing stored value
  -- 2. Outgoing wishbone bus (slave to master)
  (Signal dom a, Signal dom (WishboneS2M nBytes))
registerWbE writePriority initVal wbIn sigIn sigByteEnables = (regOut, wbOut)
 where
  regOut = registerByteAddressable initVal regIn byteEnables
  (byteEnables, wbOut, regIn) = unbundle (go <$> regOut <*> sigIn <*> sigByteEnables <*> wbIn)
  go ::
    a  ->
    Maybe a ->
    BitVector (Regs a 8) ->
    WishboneM2S nBytes addrW ->
    (BitVector (Regs a 8), WishboneS2M nBytes, a)
  go regOut0 sigIn0 sigbyteEnables0 WishboneM2S{..} =
    (byteEnables0, WishboneS2M{acknowledge, err, readData}, regIn0)
   where
    (alignedAddress, alignment) = split @_ @(addrW - 2) @2 addr
    addressRange = maxBound :: Index (Max 1 (Regs a (nBytes * 8)))
    invalidAddress = (alignedAddress > resize (pack addressRange)) || alignment /= 0
    masterActive = strobe && busCycle
    err = masterActive && invalidAddress
    acknowledge = masterActive && not err
    wbWriting = writeEnable && acknowledge
    wbAddr = unpack . resize $ pack alignedAddress :: Index (Max 1 (Regs a (nBytes * 8)))
    readData = case paddedToRegisters $ Padded regOut0 of
      RegisterBank vec -> reverse vec !! wbAddr

    wbByteEnables =
      resize . pack . reverse $ replace wbAddr busSelect (repeat @(Regs a (nBytes*8)) 0)
    sigRegIn = fromMaybe (errorX "registerWb: sigIn is Nothing when Just is expected.") sigIn0
    wbRegIn = registersToData . RegisterBank $ repeat writeData
    (byteEnables0, regIn0) = case (writePriority, isJust sigIn0, wbWriting) of
      (CircuitPriority , True , _)     -> (sigbyteEnables0, sigRegIn)
      (CircuitPriority , False, True)  -> (wbByteEnables, wbRegIn)
      (WishbonePriority, _    , True)  -> (wbByteEnables, wbRegIn)
      (WishbonePriority, True , False) -> (sigbyteEnables0, sigRegIn)
      (_               , False, False) -> (0, errorX "registerWb: register input not defined.")

-- | Register similar to 'register' with the addition that it takes a byte select signal
-- that controls which nBytes are updated.
registerByteAddressable ::
  forall dom a .
  (HiddenClockResetEnable dom, Paddable a) =>
  -- | Initial value.
  a ->
  -- | New value.
  Signal dom a ->
  -- | Byte enables that determine which nBytes of the new value are stored.
  Signal dom (ByteEnable a) ->
  -- | Stored value.
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
  -- | Incoming write operation.
  Maybe (Located maxIndex writeData) ->
  -- | Incoming byte enables.
  ByteEnable writeData ->
  -- | Per byte write operation.
  Vec (Regs writeData 8) (Maybe (LocatedByte maxIndex))
splitWriteInBytes (Just (addr, writeData)) byteSelect =
  case paddedToRegisters $ Padded writeData of
    RegisterBank vec -> splitWrites <$> unpack byteSelect <*> vec
     where
      splitWrites :: Bool -> Byte -> Maybe (LocatedByte maxIndex)
      splitWrites b bv = if b then Just (addr, bv) else Nothing

splitWriteInBytes Nothing _ = repeat Nothing

-- | Takes an address and write operation and 'bitCoerce's the addresses as follows:
-- 'bitCoerce' (address, bool)
updateAddrs ::
  (KnownNat n, 1 <= n, KnownNat m, 1 <= m) =>
  -- | An address.
  Index n
  -- | A write operation.
  -> Maybe (Index m, b)
  -- | A boolean that will be used for the addresses LSBs.
  -> SelectedBuffer
  -- |
  -- 1. Updated address
  -- 2. Write operation with updated address.
  -> (Index (n * 2), Maybe (Index (m * 2), b))
updateAddrs rdAddr (Just (i, a)) bufSelect =
  (mul2Index rdAddr bufSelect, Just (mul2Index i (flipBuffer bufSelect), a))

updateAddrs rdAddr Nothing bufSelect =
  (mul2Index rdAddr bufSelect, Nothing)
