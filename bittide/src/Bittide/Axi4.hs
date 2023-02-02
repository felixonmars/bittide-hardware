{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Bittide.Axi4 where
import Clash.Prelude
import Protocols.Wishbone
import Data.Maybe
import Protocols.Axi4.Stream
import Bittide.SharedTypes
import Clash.Sized.Internal.BitVector (popCountBV)

data ReducedAxiStreamM2S axiWidth = ReducedAxiStreamM2S
  { axisData  :: "axisData" ::: BitVector axiWidth
  , axisValid :: "axisValid" ::: Bool
  , axisLast  :: "axisLast" ::: Bool
  , axisUser  :: "axisUser" ::: Bool
  } deriving (Generic, NFDataX)


-- TODO: Remove Reduced types and switch over to types in Protocols.Axi4.Stream
data ReducedAxiStreamS2M = ReducedAxiStreamS2M Bool
  deriving (Generic, NFDataX)

data AxiByteType axiWidth addrWidth wbWidth
  = ByteEnables
  | Address (Index (DivRU addrWidth axiWidth))
  | WriteData (Index (DivRU wbWidth axiWidth))
  deriving (Generic, NFDataX, Eq)

data AxiToWbState axiWidth addrWidth wbWidth = AxiToWbState
  { axiByteType   :: AxiByteType axiWidth addrWidth wbWidth
  , wbByteEnables :: BitVector (wbWidth `DivRU` 8)
  , wbAddress     :: Vec (addrWidth `DivRU` axiWidth) (BitVector axiWidth)
  , wbWriteData   :: Vec (wbWidth `DivRU` axiWidth) (BitVector axiWidth)
  , wbValid       :: Bool
  } deriving Generic

deriving instance (KnownNat axiWidth, 1 <= axiWidth, KnownNat addrWidth, KnownNat wbWidth)
  => NFDataX (AxiToWbState axiWidth addrWidth wbWidth)

-- data WbToAxisState axiWidth wbWidth = WbToAxisState
--   { encoderCount :: Maybe (Index (DivRU wbWidth axiWidth))
--   , storedRead   :: BitVector wbWidth
--   , lastRead     :: Bool
--   } deriving (Generic, NFDataX)

type AxiWbAdapterState axiWidth addrWidth wbWidth =
  ( AxiToWbState axiWidth addrWidth wbWidth
  , Maybe (WishboneM2S addrWidth (DivRU wbWidth 8) (BitVector wbWidth))
  )

axisToWishbone ::
  forall dom axiWidth addrWidth wbWidth .
  (HiddenClockResetEnable dom, KnownNat addrWidth, KnownNat axiWidth, 1 <= axiWidth, KnownNat wbWidth) =>
  Signal dom (ReducedAxiStreamM2S axiWidth) ->
  Signal dom (WishboneS2M (BitVector wbWidth)) ->
  ( Signal dom ReducedAxiStreamS2M
  , Signal dom (WishboneM2S addrWidth (DivRU wbWidth 8) (BitVector wbWidth))
  )
axisToWishbone axisM2S wbS2M = (axisS2M, wbM2S)
 where
  (axisS2M, wbM2S) = mealyB go initState (axisM2S, wbS2M)
  initState =
    ( AxiToWbState ByteEnables 0
      (deepErrorX "axisToWishbone: Initial wishbone address undefined")
      (deepErrorX "axisToWishbone: Initial wishbone writeData undefined")
      False
    , Nothing)
  go ::
    AxiWbAdapterState axiWidth addrWidth wbWidth ->
    (ReducedAxiStreamM2S axiWidth, WishboneS2M (BitVector wbWidth)) ->
    ( AxiWbAdapterState axiWidth addrWidth wbWidth
    , (ReducedAxiStreamS2M, WishboneM2S addrWidth (DivRU wbWidth 8) (BitVector wbWidth)))

  go (axiToWbState0@AxiToWbState{..}, wbOp) (ReducedAxiStreamM2S{..}, WishboneS2M{..}) =
    ((axiToWbState2, wbOpNext), output)
   where
    nextAxiByteType = case (axisLast, axisValid, wbValid, axiByteType) of
      (True , True    , _    , _          )      -> ByteEnables
      (_    , _    , True , ByteEnables)      -> ByteEnables
      (_    , False, _    , _          )      -> axiByteType

      (False, True , False, ByteEnables)      -> Address 0
      (False, True , _    , Address n  )
        | n == maxBound && wbByteEnables == 0 -> ByteEnables
        | n == maxBound                       -> WriteData 0
        | otherwise                           -> Address (succ n)
      (False, True , _    , WriteData n)
        | n == maxBound                       -> ByteEnables
        | otherwise                           -> WriteData (succ n)

    wbLastByte = case (axisValid, axiByteType, wbByteEnables == 0) of
      (True, WriteData n, _)  -> n == maxBound
      (True, Address n, True) -> n == maxBound
      _                       -> False

    wbTerminated = acknowledge || err || retry
    (wbOpNext, nextWbValid)
      | isJust wbOp && not wbTerminated = (wbOp, wbValid || wbLastByte)
      | wbValid && (isNothing wbOp || wbTerminated) = (Just $ (emptyWishboneM2S @addrWidth @(BitVector  wbWidth))
        { busCycle = True
        , strobe = True
        , busSelect = wbByteEnables
        , writeData = resize $ pack wbWriteData
        , writeEnable = or $ False :> unpack wbByteEnables
        , addr = resize $ pack wbAddress}
        , False)
      | otherwise = (Nothing, wbValid || wbLastByte)


    axiToWbState1 = case (axiByteType, axisValid) of
      (ByteEnables, True) -> axiToWbState0{wbByteEnables = resize axisData}
      (Address n  , True) -> axiToWbState0{wbAddress     = replace n axisData (reverse wbAddress)}
      (WriteData n, True) -> axiToWbState0{wbWriteData   = replace n axisData (reverse wbWriteData)}
      (_, False)          -> axiToWbState0

    axiToWbState2 = axiToWbState1{axiByteType = nextAxiByteType, wbValid = nextWbValid}

    output =
      ( ReducedAxiStreamS2M (not wbValid || axiByteType /= ByteEnables)
      , fromMaybe emptyWishboneM2S wbOp)

{-# NOINLINE axisToWishbone #-}

type EndOfPacket = Bool
type BufferFull = Bool

data WbAxisRxBufferState fifoDepth nBytes = WbAxisRxBufferState
  { readingFifo :: Bool
  , packetLength :: Index (fifoDepth * nBytes + 1)
  , writeCounter :: Index fifoDepth
  , packetComplete :: Bool
  , bufferFull :: Bool
  } deriving (Generic, NFDataX)

-- axisUpscale ::
--   (DataWidth confA ~ scaleFactor * DataWidth confB) =>
--   Signal dom (Maybe (Axi4StreamM2S confA userType)) ->
--   Signal dom Axi4StreamS2M ->
--   (Signal dom Axi4StreamS2M, Signal dom (Maybe (Axi4StreamM2S confB userType)))
-- axisUpscale = curry (mealyB go initState)
--  where
--   initState = _
--   go (oldCounter, storedBytes) (smallM2S, bigS2M) = ((newCounter, nextStoredBytes), (smallS2M, bigM2S))
--    where
--     smallS2M = _
--     bigM2S = _
--     nBytesIn = _
--     nBytesOut = _
--     -- smallHandShake = smallReady && isJust smallM2S
--     enoughBytes = newCounter == maxBound
--     newCounter
--       | enoughBytes = 0
--       | otherwise   = satSucc SatWrap oldCounter

--     intermediateBytes = storedBytes ++ _tdata smallM2S
--     nextStoredBytes = dropI intermediateBytes

wbAxisRxBuffer ::
  forall dom wbAddrW nBytes fifoDepth conf axiUserType .
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW, 2 <= wbAddrW
  , KnownNat nBytes, 1 <= nBytes
  , 1 <= fifoDepth
  , 1 <= nBytes * fifoDepth
  , DataWidth conf ~ nBytes) =>
  SNat fifoDepth ->
  "wbm2s" ::: Signal dom (WishboneM2S wbAddrW nBytes (Bytes nBytes)) ->
  "axim2s" ::: Signal dom (Maybe (Axi4StreamM2S conf axiUserType)) ->
  "clearinterrupts" ::: Signal dom (EndOfPacket, BufferFull) ->
  "" :::
  ( "" ::: Signal dom (WishboneS2M (Bytes nBytes))
  , "" ::: Signal dom Axi4StreamS2M
  , "" ::: Signal dom (EndOfPacket, BufferFull)
  )
wbAxisRxBuffer SNat wbM2S axisM2S statusClearSignal = (wbS2M, axisS2M, statusReg)
 where
  fifoOut =
    blockRamU NoClearOnReset (SNat @fifoDepth)
    (const $ errorX "wbAxisRxBuffer: reset function undefined")
    bramAddr bramWrite
  (wbS2M, axisS2M, bramAddr, bramWrite, statusReg) =
    mealyB go initState (wbM2S, axisM2S, fifoOut, statusClearSignal)
  initState = WbAxisRxBufferState
    { readingFifo = False
    , packetLength = 0
    , writeCounter = 0
    , packetComplete = False
    , bufferFull = False
    }
  go ::
    WbAxisRxBufferState fifoDepth nBytes ->
    ( WishboneM2S wbAddrW nBytes (Bytes nBytes)
    , Maybe (Axi4StreamM2S conf axiUserType)
    , Bytes nBytes, (EndOfPacket, BufferFull)
    ) ->
    ( WbAxisRxBufferState fifoDepth nBytes
    , ( WishboneS2M (Bytes nBytes)
      , Axi4StreamS2M, Index fifoDepth
      , Maybe (Index fifoDepth, Bytes nBytes)
      , (EndOfPacket, BufferFull)
      )
    )
  go
    WbAxisRxBufferState{..}
    (WishboneM2S{..}, maybeAxisM2S, wbData, clearStatus)
    = (newState, output)
   where
    masterActive = busCycle && strobe
    (alignedAddress, alignment) = split @_ @(wbAddrW - 2) @2 addr

    packetLengthAddress = maxBound - 1
    statusAddress       = maxBound :: Index (fifoDepth + 2)
    internalAddress = (bitCoerce $ resize alignedAddress) :: Index (fifoDepth + 2)

    err = masterActive && (alignment /= 0 || alignedAddress > resize (pack statusAddress))

    statusBV = pack (packetComplete, bufferFull)
    wbHandshake = masterActive && not err

    (readData, nextReadingFifo, wbAcknowledge)
      | internalAddress == packetLengthAddress = (resize $ pack packetLength, False, wbHandshake)
      | internalAddress == statusAddress       = (resize statusBV, False, wbHandshake)
      | otherwise                              = (wbData, wbHandshake && not readingFifo, wbHandshake && readingFifo)

    axisReady = not (packetComplete || bufferFull)
    axisHandshake = axisReady && isJust maybeAxisM2S

    output =
      ( (emptyWishboneS2M @(Bytes nBytes)){readData, err, acknowledge = wbAcknowledge}
      , Axi4StreamS2M axisReady
      , resize internalAddress
      , if axisHandshake then (writeCounter,) . pack . _tdata <$> maybeAxisM2S else Nothing
      , (packetComplete, bufferFull)
      )

    -- Next state
    (nextPacketComplete, nextBufferFull)
      | wbAcknowledge && writeEnable && internalAddress == statusAddress
        = unpack $ (\ a b -> a `xor` (a .&. b)) statusBV (resize writeData)
      | axisHandshake = (packetComplete || maybe False _tlast maybeAxisM2S, bufferFull || writeCounter == maxBound)
      | otherwise = unpack $ statusBV `xor` pack clearStatus

    nextWriteCounter
      | axisHandshake  = satSucc SatBound writeCounter
      | packetComplete || bufferFull = 0
      | otherwise      = writeCounter

    bytesInStream = maybe (0 :: Index (nBytes + 1)) (leToPlus @1 @nBytes popCountBV . pack ._tkeep) maybeAxisM2S

    nextPacketLength
      | wbAcknowledge && writeEnable && internalAddress == packetLengthAddress = unpack $ resize writeData
      | axisHandshake                                         = satAdd SatBound packetLength (bitCoerce $ resize bytesInStream)
      | otherwise                                             = packetLength

    newState = WbAxisRxBufferState
      { readingFifo = nextReadingFifo
      , packetLength = nextPacketLength
      , writeCounter = nextWriteCounter
      , packetComplete = nextPacketComplete
      , bufferFull = nextBufferFull
      }

wbAxisTxBuffer ::
  forall dom addrW nBytes fifoDepth conf .
  ( HiddenClockResetEnable dom
  , 1 <= fifoDepth
  , DataWidth conf ~ nBytes
  , 2 <= addrW
  , KnownNat addrW
  , KnownAxi4StreamConfig conf) =>
  SNat fifoDepth ->
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  Signal dom Axi4StreamS2M ->
  ( Signal dom (WishboneS2M (Bytes nBytes))
  , Signal dom (Axi4StreamM2S conf Bool))
wbAxisTxBuffer SNat wbM2S axisS2M = (wbS2M, axisM2S)
 where
  (wbS2M, axisM2S, bramWrite, readAddr) = mealyB go initState (wbM2S, axisS2M, bramOut)
  initState = (0,0,False)
  bramOut =
    blockRamU NoClearOnReset (SNat @fifoDepth)
    (const $ errorX "wbAxisRxBuffer: reset function undefined")
    readAddr bramWrite
  go (readCounter, wordCount, axisValid) (WishboneM2S{..}, Axi4StreamS2M{..}, (bramKeeps,bramData)) = (newState, output)
   where
    masterActive = busCycle && strobe
    (alignedAddress, alignment) = split @_ @(addrW - 2) @2 addr
    wordCounterAddress = maxBound :: Index (fifoDepth + 1)
    err = masterActive && (alignment /= 0 || alignedAddress > resize (pack wordCounterAddress))
    acknowledge = masterActive && not err
    validWrite = acknowledge && writeEnable
    internalAddress = (unpack $ resize alignedAddress) :: Index (fifoDepth + 1)

    newState = (readCounterNext, wordCountNext, axisValidNext)
    newWordCount = validWrite && internalAddress == wordCounterAddress
    lastTransmit = axisHandshake && readCounter == wordCount

    readCounterNext
      | lastTransmit  = 0
      | axisHandshake = satSucc SatError readCounter
      | otherwise     = readCounter

    wordCountNext
      | newWordCount = unpack $ resize writeData
      | lastTransmit = 0
      | otherwise    = wordCount

    writeOp
      | validWrite && internalAddress < maxBound
      = Just (resize @_ @_ @fifoDepth internalAddress, (busSelect, writeData))
      | otherwise = Nothing

    axisValidNext
      | newWordCount = True
      | lastTransmit = False
      | otherwise    = axisValid

    axisHandshake = axisValid && _tready
    readData = resize $ pack wordCount
    retry = False
    stall = False

    _tdata = reverse $ unpack bramData
    _tkeep = unpack bramKeeps
    _tstrb = repeat True
    _tlast = axisHandshake && wordCount == 0
    _tid = 0
    _tdest = 0
    _tuser = False
    output =
      ( WishboneS2M{acknowledge, err, readData,retry, stall}
      , Axi4StreamM2S{_tdata, _tkeep, _tstrb, _tlast, _tid, _tdest, _tuser}
      , writeOp
      , if axisHandshake then readCounterNext else readCounter
      )
