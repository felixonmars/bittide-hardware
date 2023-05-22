-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Axi4 where

import Clash.Prelude

import Protocols
import Protocols.Axi4.Stream
import Data.Maybe

import Bittide.Extra.Maybe
import Bittide.SharedTypes

{-# NOINLINE axisFromByteStream #-}
axisFromByteStream ::
  forall dom dataWidth idWidth destWidth userType .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
    (Axi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
axisFromByteStream = Circuit (mealyB go initState . addEnabledSignal .bundle)
 where
  initState :: (Vec (dataWidth - 1) (Unsigned 8, Bool, Bool))
  initState = (repeat initTempAxi, False)
  initTempAxi = (0, False, False)
  go (storedBytes, _) (False, _) = (storedBytes, (Axi4StreamS2M{_tready = False}, Nothing))
  go (storedBytes, storedTLast) (True, (Nothing, _)) = (storedBytes, (Axi4StreamS2M{_tready = True}, Nothing))
  go (storedBytes, storedTLast) (True, (Just Axi4StreamM2S{..}, Axi4StreamS2M{_tready})) =
    (nextStoredBytes, (Axi4StreamS2M{_tready = smallReady}, bigM2S))
   where
    bigM2S
      | not upscaleDone = Nothing
      | otherwise       = Just $ Axi4StreamM2S
        { _tdata = newData
        , _tkeep = newKeeps
        , _tstrb = newStrobes
        , _tuser = _tuser
        , _tid   = _tid
        , _tdest = _tdest
        , _tlast = _tlast
        }

    (newData, newKeeps, newStrobes) = unzip3 $ takeI storedBytes
    upscaleDone = newKeeps == repeat True || storedTLast
    smallReady  = upscaleDone || _tready
    interMediateBytes0 = if smallReady then repeat Nothing else take (SNat @dataWidth) storedBytes
    intermediateBytes = compressVec (interMediateBytes0 ++ zip3 (orNothing <$> _tdata <*> _tkeep) _tkeep _tstrobe)

    nextStoredBytes
      | smallReady  = initState
      | upscaleDone = storedBytes
      | otherwise   = tail intermediateBytes

compressVec :: KnownNat n => Vec n (Maybe a) -> Vec n (Maybe a)
compressVec = vfold (const prefLeft)
 where
  prefLeft y xs  = let (y',xs') = mapAccumL justLeft y xs in xs' :< y'
  justLeft a b = if isJust a then (a,b) else (b,a)


{-# NOINLINE axisToByteStream #-}
axisToByteStream ::
  forall dom dataWidth idWidth destWidth userType .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  , Eq userType
  , NFDataX userType) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
axisToByteStream = Circuit (mealyB go (0 :: Index dataWidth))
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
    lastValidByteIndex = fromMaybesR 0 (orNothing <$> _tkeep <*> indicesI)
    newCounter
      | _tready && lastByte = 0
      | _tready = satSucc SatWrap oldCounter
      | otherwise = oldCounter
