-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Bittide.Axi4 where

import Clash.Prelude

import Data.Maybe
import Protocols.Axi4.Stream

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
