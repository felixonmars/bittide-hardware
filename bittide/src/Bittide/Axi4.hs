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

{-# NOINLINE axisFromByteStream #-}
axisFromByteStream ::
  forall dom confA confB userType .
  ( HiddenClockResetEnable dom, DataWidth confA ~ 1, KnownAxi4StreamConfig confB
  , 1 <= DataWidth confB
  , DestWidth confA ~ DestWidth confB
  , IdWidth confA ~ IdWidth confB
  , Eq userType, NFDataX userType) =>
  "axisByteM2S"  ::: Signal dom (Maybe (Axi4StreamM2S confA userType)) ->
  "axisWordS2M" ::: Signal dom Axi4StreamS2M ->
  "" :::
    ( "AxisByteS2M" ::: Signal dom Axi4StreamS2M
    , "AxisWordM2S" ::: Signal dom (Maybe (Axi4StreamM2S confB userType)))
axisFromByteStream = curry (mealyB go initState)
 where
  initState :: (Index (DataWidth confB), Vec (DataWidth confB - 1) (Unsigned 8, Bool, Bool), userType)
  initState = (0, repeat initTempAxi, deepErrorX "axisFromByteStream: Initial user signal is undefined.")
  initTempAxi = (deepErrorX "axisFromByteStream: Initial data undefined.", False, deepErrorX "axisFromByteStream: Initial strobe undefined.")
  go state (Nothing, _) = (state, (Axi4StreamS2M{_tready = True}, Nothing))
  go (oldCounter, storedBytes, prevUser) (Just Axi4StreamM2S{..}, Axi4StreamS2M{_tready}) = ((newCounter, nextStoredBytes, _tuser), (smallS2M, bigM2S))
   where
    smallS2M = Axi4StreamS2M{_tready = smallReady}
    bigM2S
      | not upscaleDone = Nothing
      | otherwise       = Just $ Axi4StreamM2S
        { _tdata = newData
        , _tkeep = newKeeps
        , _tstrb = newStrobes
        , _tuser = if oldCounter > 0 then prevUser else _tuser
        , _tid   = _tid
        , _tdest = _tdest
        , _tlast = _tlast}
    (newData, newKeeps, newStrobes) = unzip3 intermediateBytes

    userValid = oldCounter == 0 || prevUser == _tuser
    upscaleDone = oldCounter == maxBound || not userValid || _tlast
    smallReady  = not upscaleDone || _tready
    newCounter
      | upscaleDone && _tready && _tready = 0
      | upscaleDone = oldCounter
      | otherwise   = satSucc SatWrap oldCounter

    intermediateBytes = replace oldCounter (head _tdata, head _tkeep, head _tstrb)
      (storedBytes :< initTempAxi)

    nextStoredBytes
      | upscaleDone && _tready = repeat initTempAxi
      | otherwise              = init intermediateBytes

{-# NOINLINE axisToByteStream #-}
axisToByteStream ::
  forall dom confA confB userType .
  ( HiddenClockResetEnable dom, DataWidth confB ~ 1, KnownAxi4StreamConfig confA
  , 1 <= DataWidth confA
  , DestWidth confA ~ DestWidth confB, IdWidth confA ~ IdWidth confB) =>
  "AxisWordM2S" ::: Signal dom (Maybe (Axi4StreamM2S confA userType)) ->
  "AxisByteS2M" ::: Signal dom Axi4StreamS2M ->
  "" ::: ( "AxisWordS2M" ::: Signal dom Axi4StreamS2M, "AxisByteM2S" ::: Signal dom (Maybe (Axi4StreamM2S confB userType)))
axisToByteStream = curry (mealyB go (0 :: Index (DataWidth confA)))
 where
  go state (Nothing, _) = (state, (Axi4StreamS2M{_tready = True}, Nothing))
  go oldCounter (Just Axi4StreamM2S{..}, Axi4StreamS2M{_tready}) = (newCounter, (bigS2M, smallM2S))
   where
    bigS2M = Axi4StreamS2M{_tready = _tready && lastByte}
    lastByte = oldCounter == lastValidByteIndex
    smallM2S = Just $ Axi4StreamM2S
        { _tdata = outData :> Nil
        , _tkeep = outKeep :> Nil
        , _tstrb = outStrb :> Nil
        , _tuser = _tuser
        , _tid   = _tid
        , _tdest = _tdest
        , _tlast = _tlast && lastByte}
    (outData, outKeep, outStrb) = zip3 _tdata _tkeep _tstrb !! oldCounter
    lastValidByteIndex = fromMaybe 0 $ fold (\ a b -> if isJust b then b else a) $ ((\a b -> if a then Just b else Nothing) <$> _tkeep <*> indicesI) :< Nothing
    newCounter
      | _tready && lastByte = 0
      | _tready = satSucc SatWrap oldCounter
      | otherwise = oldCounter
